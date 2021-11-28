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
  , setCommand
  , step
  ) where

import           Relude

import           Utils

import           Control.Lens
import qualified Data.Set                      as Set
import qualified Data.Vector.Fixed             as Vec
import           System.Random


data Component = CanMove | IsPlayer deriving (Eq, Ord)

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


data Command = Noop | North | East | South | West


data GameState = GameState
  { gameRNG   :: StdGen
  , _entities :: [Entity]
  , _command  :: Command
  }

makeLenses ''GameState

mkGameState :: Command -> IO GameState
mkGameState cmd = do
  rngX <- getStdGen
  let (x, rngY) = randomCoord rngX
  let (y, rng)  = randomCoord rngY

  return $ GameState { gameRNG   = rng
                     , _entities = [Entity x y [CanMove, IsPlayer]]
                     , _command  = cmd
                     }
  where randomCoord = randomR (0 :: Int, 17)

setCommand :: Command -> GameState -> GameState
setCommand = set command


data System = System
  { componentSet :: Set Component
  , everyTick    :: Command -> Entity -> GameState -> GameState
  }

-- runs a system's everyTick over all entities
runSystemET :: System -> GameState -> GameState
runSystemET s gs = (foldl' (.) id . map (everyTick s $ gs ^. command))
  (filter (Set.isSubsetOf (componentSet s) . (^. components)) (gs ^. entities))
  gs

systems :: [System]
systems =
  [ -- move player
    System { componentSet = [CanMove, IsPlayer]
           -- TODO placeholder - should respond to commands
           , everyTick = \c e -> over entities $ replace e (fromMoveCommand c e)
           }
  ]
 where
  fromMoveCommand c = case c of
    North -> over posY (subtract 1)
    East  -> over posX (+ 1)
    South -> over posY (+ 1)
    West  -> over posX (subtract 1)
    _     -> id


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid = flip compose mkEntityGrid . map setEntity . (^. entities)
  where setEntity e = mset (e ^. posX) (e ^. posY) (Just e)


executeStep :: GameState -> GameState
executeStep = compose . map runSystemET $ systems

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
