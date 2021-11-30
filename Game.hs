{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game
  ( mkEntityGrid
  , mkGameState
  , Entity
  , EntityGrid
  , Command(..)
  , GameState
  , Tile(..)
  , represent
  , setCommand
  , step
  ) where

import           Relude

import           Utils

import           Control.Lens
import           Data.Default
import qualified Data.Set                      as Set
import qualified Data.Vector.Fixed             as Vec
import           System.Random


data Component = CanMove | IsPlayer | IsWall deriving (Eq, Ord)

data Entity = Entity
  { _posX       :: Int
  , _posY       :: Int
  , _components :: Set Component
  }
  deriving Eq

makeLenses ''Entity


type EntityGrid = Matrix (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Matrix . Vec.replicate . Vec.replicate $ Nothing


data Command = Noop | North | East | South | West deriving (Eq, Ord, Show)


data GameState = GameState
  { gameRNG   :: StdGen
  , _entities :: [Entity]
  , _command  :: Command
  }

makeLenses ''GameState

mkGameState :: Command -> IO GameState
mkGameState cmd = do
  rng <- getStdGen
  let (player, rng')  = entityAtRandomPos rng [CanMove, IsPlayer]
  let (wall, rng'')   = entityAtRandomPos rng' [IsWall]
  let (wall', rng''') = entityAtRandomPos rng'' [IsWall]

  return $ GameState { gameRNG   = rng'''
                     , _entities = [player, wall, wall']
                     , _command  = cmd
                     }
 where
  randomCoord = randomR (0 :: Int, 17)
  entityAtRandomPos rng comps =
    let (x, rngX) = randomCoord rng
        (y, rngY) = randomCoord rngX
    in  (Entity x y comps, rngY)


setCommand :: Command -> GameState -> GameState
setCommand = set command


data Tile = EmptyTile | PlayerTile | WallTile deriving Eq

data System = System
  { componentSet :: Set Component
  , everyTick    :: Command -> Entity -> GameState -> GameState
  , buildRepr    :: Entity -> Tile -> Tile
  }

instance Default System where
  def =
    System { componentSet = [], everyTick = \_ _ -> id, buildRepr = const id }

-- runs a system's everyTick over all entities
runSystemET :: System -> GameState -> GameState
runSystemET s gs = (foldl' (.) id . map (everyTick s $ gs ^. command))
  (filter (Set.isSubsetOf (componentSet s) . (^. components)) (gs ^. entities))
  gs

systems :: [System]
systems =
  [ -- render player
    def { componentSet = [IsPlayer], buildRepr = \_ _ -> PlayerTile }
    -- move player
  , def
    { componentSet = [CanMove, IsPlayer]
           -- TODO placeholder - should respond to commands
    , everyTick    = \c e g ->
      (over entities $ replace e $ fromMoveCommand c (g ^. entities) e) g
    }
    -- render wall
  , def { componentSet = [IsWall], buildRepr = \_ _ -> WallTile }
  ]
 where
   -- this has to prevent entities from walking on walls
   -- that is why it is so complicated
   -- BUG can't move above or below walls
  fromMoveCommand c es e =
    let newEntity = case c of
          North -> over posY (subtract 1) e
          East  -> over posX (+ 1) e
          South -> over posY (+ 1) e
          West  -> over posX (subtract 1) e
          _     -> e
        newPos = (newEntity ^. posX, newEntity ^. posY)
    in  if any
             (\e' ->
               Set.member IsWall (e' ^. components)
                 && ((e' ^. posX, e ^. posY) == newPos)
             )
             es
          then e
          else newEntity

represent :: Entity -> Tile
represent e =
  ( foldl' (.) id
    . map (($ e) . buildRepr)
    . filter (flip Set.isSubsetOf (e ^. components) . componentSet)
    $ systems
    )
    EmptyTile


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid = flip compose mkEntityGrid . map setEntity . (^. entities)
  where setEntity e = mset (e ^. posX) (e ^. posY) (Just e)


executeStep :: GameState -> GameState
executeStep = (compose . map runSystemET) systems

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
