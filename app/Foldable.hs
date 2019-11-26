module Foldable where

import Prelude hiding (Foldable, foldl, foldr, sum)

class Foldable t where
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldr :: (a -> b -> b) -> b -> t a -> b

sum :: (Num a, Foldable t) => t a -> a
sum = foldl (+) 0

len :: Foldable t => t a -> Int
len = foldl (\x _ -> x + 1) 0

isEmpty :: Foldable t => t a -> Bool
isEmpty = foldl (\_ _ -> False) True

contains :: (Eq a, Foldable t) => a -> t a -> Bool
contains a = foldl (\c x -> c || x == a) False
