{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import           Relude
import           Relude.Unsafe                  ( (!!) )

import           Control.Lens            hiding ( Context )
import qualified Data.Vector.Fixed             as Vec
import           Data.Vector.Fixed.Boxed        ( Vec )
import           Network.Wreq.Session           ( Session )
import           System.Random


data Context = Context
  { ctxSession   :: Session
  , ctxAPIToken  :: Text
  , ctxWSToken   :: Text
  , ctxChannelID :: Text
  }

type RogueM = ReaderT Context IO


compose :: [a -> a] -> a -> a
compose = flip $ foldl' (&)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\o -> if o == x then y else o)

randomChoice :: [a] -> StdGen -> a
randomChoice xs rng = xs !! n where n = fst $ randomR (0, length xs - 1) rng

(&&$), (||$) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&$) f g x = f x && g x
(||$) f g x = f x || g x


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

mget :: Int -> Int -> Matrix a -> a
mget x y = (!! x) . (!! y) . m2l

mset :: Int -> Int -> a -> Matrix a -> Matrix a
mset x y e = l2m . over (ix y) (& ix x .~ e) . m2l
