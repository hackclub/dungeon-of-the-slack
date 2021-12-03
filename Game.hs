{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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


data Component = CanMove | IsPlayer | IsWall deriving (Eq, Ord, Show)


data Entity = Entity
  { _posX       :: Int
  , _posY       :: Int
  , _components :: Set Component
  }
  deriving (Eq, Show)

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
  -- let (player, rng') = entityAtRandomPos rng [CanMove, IsPlayer]
  let (rng', rngWalls) = split rng

  return $ GameState { gameRNG   = rng'
                     , _entities = mkWalls rngWalls []
                     , _command  = cmd
                     }
 where
  randomCoord = randomR (1 :: Int, 22)

  mkWalls :: StdGen -> [Entity] -> [Entity]
  mkWalls rng es = mkWalls' (25 :: Integer) rng es

  -- TODO code is ugly; should probably refactor slightly
  mkWalls' 0 _ es = es
  mkWalls' n rng es =
    let (x, rng'  ) = randomCoord rng
        (y, newRNG) = randomCoord rng'
        adjEntities isX = filter
          (\e -> abs (view pos1 e - dim1) < 3 && view pos2 e - dim2 == 0)
            -- TODO which way leads to better-looking maps? get feedback
          es
           where
            pos1 = if isX then posX else posY
            dim1 = if isX then x else y
            pos2 = if isX then posY else posX
            dim2 = if isX then y else x
        constrainWalls isMin isX =
          (\case
              []    -> if isMin then 0 else 23
              e : _ -> e ^. pos1
            )
            . (if isMin then reverse else id)
            . sortOn (view pos1)
            . filter (flip (if isMin then (<) else (>)) dim1 . view pos1)
            . filter ((== dim2) . view pos2)
           where
            dim1 = if isX then x else y
            dim2 = if isX then y else x
            pos1 = if isX then posX else posY
            pos2 = if isX then posY else posX
        minX     = constrainWalls True True es
        maxX     = constrainWalls False True es
        minY     = constrainWalls True False es
        maxY     = constrainWalls False False es
        newWalls = if even n
          then [ Entity x' y [IsWall] | x' <- [minX .. maxX] ]
          else [ Entity x y' [IsWall] | y' <- [minY .. maxY] ]
    in  if null (adjEntities $ odd n)
          then mkWalls' (n + 1) newRNG (newWalls ++ es)
          else mkWalls' (n - 1) newRNG es

  -- entityAtRandomPos rng comps =
  --   let (x, rngX) = randomCoord rng
  --       (y, rngY) = randomCoord rngX
  --   in  (Entity x y comps, rngY)


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
    , everyTick    = \c e g ->
      (over entities $ replace e $ fromMoveCommand c (g ^. entities) e) g
    }
    -- render wall
  , def { componentSet = [IsWall], buildRepr = \_ _ -> WallTile }
  ]
 where
   -- this has to prevent entities from walking on walls
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
                 && ((e' ^. posX, e' ^. posY) == newPos)
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
