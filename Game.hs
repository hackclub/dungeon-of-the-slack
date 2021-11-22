{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Game
  ( g2l
  , mkEntityGrid
  , mkGameState
  , EntityGrid
  , GameState
  , step
  ) where

import           Relude

import           Utils                          ( compose )

import           Control.Lens
import qualified Data.Vector.Fixed             as Vec
import           Data.Vector.Fixed.Boxed        ( Vec )
import           System.Random


newtype Grid a = Grid { gridVec :: Vec 18 (Vec 18 a) }

instance Functor Grid where
  fmap f = Grid . (Vec.map . Vec.map) f . gridVec

-- maybe this should be some typeclass instance
g2l :: Grid a -> [[a]]
g2l = Vec.toList . Vec.map Vec.toList . gridVec

l2g :: [[a]] -> Grid a
l2g = Grid . Vec.fromList . map Vec.fromList

gset :: Int -> Int -> a -> Grid a -> Grid a
gset x y e = l2g . over (ix y) (& ix x .~ e) . g2l


type EntityGrid = Grid (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Grid . Vec.replicate . Vec.replicate $ Nothing


data Entity = Entity
  { entityX :: Int
  , entityY :: Int
  }

data GameState = GameState
  { gsRNG    :: StdGen
  , entities :: [Entity]
  }

mkGameState :: IO GameState
mkGameState = do
  rng <- getStdGen
  let (entityX', rng')  = randomR (0 :: Int, 17) rng
  let (entityY', rng'') = randomR (0 :: Int, 17) rng'
  return $ GameState { gsRNG = rng'', entities = [Entity entityX' entityY'] }


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid = flip compose mkEntityGrid . map setEntity . entities
  where setEntity e = gset (entityX e) (entityY e) (Just e)


executeStep :: GameState -> GameState
executeStep = id -- TODO placeholder

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
