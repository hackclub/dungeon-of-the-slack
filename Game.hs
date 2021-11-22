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


newtype Grid a = Grid { gridVec :: Vec 18 (Vec 18 a) }

instance Functor Grid where
  fmap f = Grid . (Vec.map . Vec.map) f . gridVec

-- maybe this should be some typeclass instance
g2l :: Grid a -> [[a]]
g2l = Vec.toList . Vec.map Vec.toList . gridVec

l2g :: [[a]] -> Grid a
l2g = Grid . Vec.fromList . map Vec.fromList

gset :: Int -> Int -> a -> Grid a -> Maybe (Grid a)
gset x y e = Just . l2g . over (ix y) (& ix x .~ e) . g2l


type EntityGrid = Grid (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Grid . Vec.replicate . Vec.replicate $ Nothing


data Entity = Entity
  { entityX :: Int
  , entityY :: Int
  }

newtype GameState = GameState
  { entities :: [Entity]
  }

mkGameState :: GameState
mkGameState = GameState [Entity 0 0] -- TODO placeholder


getGrid :: GameState -> EntityGrid
getGrid =
  -- TODO this is bad!
  -- an out-of-bounds entity will just empty the grid
  fromMaybe mkEntityGrid
    . flip compose (Just mkEntityGrid)
    . map setEntity
    . entities
  where setEntity e = (=<<) $ gset (entityX e) (entityY e) (Just e)


executeStep :: GameState -> GameState
executeStep = id -- TODO placeholder

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
