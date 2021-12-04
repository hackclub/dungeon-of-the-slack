{-# LANGUAGE DataKinds #-}

module Utils where

import           Control.Lens
import qualified Data.Vector.Fixed             as Vec
import           Data.Vector.Fixed.Boxed        ( Vec )


compose :: [a -> a] -> a -> a
compose = flip $ foldl (&)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\o -> if o == x then y else o)


-- Matrix
---------

matrixSize :: Int
matrixSize = 14

-- yes i know hardcoding the dims here is bad
newtype Matrix a = Matrix { fromMatrix :: Vec 14 (Vec 14 a) }

instance Functor Matrix where
  fmap f = Matrix . (Vec.map . Vec.map) f . fromMatrix

-- maybe this should be some typeclass instance
m2l :: Matrix a -> [[a]]
m2l = Vec.toList . Vec.map Vec.toList . fromMatrix

l2m :: [[a]] -> Matrix a
l2m = Matrix . Vec.fromList . map Vec.fromList

mset :: Int -> Int -> a -> Matrix a -> Matrix a
mset x y e = l2m . over (ix y) (& ix x .~ e) . m2l
