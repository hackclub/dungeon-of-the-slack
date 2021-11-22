module Utils where

compose :: [a -> a] -> a -> a
compose = flip $ foldl (\x f -> f x)
