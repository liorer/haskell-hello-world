module Foldable where

import Prelude hiding (Foldable, foldl, foldr, sum)

class Foldable t where
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldr :: (a -> b -> b) -> b -> t a -> b

sum :: Foldable t => t Int -> Int
sum = foldl (+) 0

len :: Foldable t => t a -> Int
len = foldl (\x _ -> x + 1) 0

-- isEmpty
-- contains
