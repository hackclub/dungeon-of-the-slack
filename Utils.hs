{-# LANGUAGE DataKinds #-}

module Utils where

import           Control.Lens
import qualified Data.Vector.Fixed             as Vec
import           Data.Vector.Fixed.Boxed        ( Vec )


compose :: [a -> a] -> a -> a
compose = flip $ foldl (&)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\o -> if o == x then y else o)


-- Grid
-------

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
