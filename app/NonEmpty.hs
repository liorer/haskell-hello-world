{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module NonEmpty where

import Prelude (Show(..))
import List
import Foldable

data NonEmpty a = NonEmpty a (List a)
    deriving Show

instance Foldable NonEmpty where
    foldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    foldl f z (NonEmpty h t) = foldl f (f z h) t

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (NonEmpty h t) = f h (foldr f z t)

head :: NonEmpty a -> a
head (NonEmpty h _) = h

tail :: NonEmpty a -> List a
tail (NonEmpty _ t) = t
