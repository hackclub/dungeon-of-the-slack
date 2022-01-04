{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import           Relude
import           Relude.Unsafe                  ( (!!) )

import           Control.Lens            hiding ( Context )
import           Control.Monad.Random
import qualified Data.Vector.Fixed             as Vec
import           Data.Vector.Fixed.Boxed        ( Vec )


compose :: [a -> a] -> a -> a
compose = flip $ foldl' (&)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\o -> if o == x then y else o)

(&&$), (||$) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&$) f g x = f x && g x
(||$) f g x = f x || g x


-- random
---------

type RandM = Rand StdGen

randomChoice :: [a] -> RandM a
randomChoice xs = getRandomR (0, length xs - 1) <&> (xs !!)


-- matrix
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

mget :: Int -> Int -> Matrix a -> a
mget x y = (!! x) . (!! y) . m2l

mset :: Int -> Int -> a -> Matrix a -> Matrix a
mset x y e = l2m . over (ix y) (& ix x .~ e) . m2l
