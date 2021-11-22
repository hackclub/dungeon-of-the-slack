module Utils where

import           Data.Function                  ( (&) )

compose :: [a -> a] -> a -> a
compose = flip $ foldl (&)
