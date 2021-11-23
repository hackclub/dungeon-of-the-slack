{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( mkEntityGrid
  , mkGameState
  , EntityGrid
  , GameState
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


type EntityGrid = Grid (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Grid . Vec.replicate . Vec.replicate $ Nothing


data GameState = GameState
  { gameRNG   :: StdGen
  , _entities :: [Entity]
  }

makeLenses ''GameState

mkGameState :: IO GameState
mkGameState = do
  rngX <- getStdGen
  let (x, rngY) = randomCoord rngX
  let (y, rng)  = randomCoord rngY

  return $ GameState { gameRNG = rng, _entities = [Entity x y playerComps] }
 where
  randomCoord = randomR (0 :: Int, 17)
  playerComps = Set.fromList [CanMove, IsPlayer]


data System = System
  { componentSet :: Set Component
  , everyTick    :: Entity -> GameState -> GameState
  }

-- runs a system's everyTick over all entities
runSystemET :: System -> GameState -> GameState
runSystemET s gs = (foldl' (.) id . map (everyTick s))
  (filter (flip Set.isSubsetOf (componentSet s) . (^. components))
          (gs ^. entities)
  )
  gs


systems :: [System]
systems =
  [ -- move player
    System { componentSet = Set.fromList [CanMove, IsPlayer]
           , everyTick    = \e -> over entities $ replace e (over posX (+ 1) e)
           }
  ]


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid = flip compose mkEntityGrid . map setEntity . (^. entities)
  where setEntity e = gset (e ^. posX) (e ^. posY) (Just e)


executeStep :: GameState -> GameState
executeStep = compose . map runSystemET $ systems

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
