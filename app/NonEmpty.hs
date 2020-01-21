{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module NonEmpty where

import Prelude (Show(..))
import Semigroup
import List hiding(concat)
import Foldable

data NonEmpty a = NonEmpty a (List a)
    deriving Show

instance Foldable NonEmpty where
    foldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    foldl f z (NonEmpty h t) = foldl f (f z h) t

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (NonEmpty h t) = f h (foldr f z t)

instance Semigroup (NonEmpty a) where
    (<>) = concat

head :: NonEmpty a -> a
head (NonEmpty h _) = h

tail :: NonEmpty a -> List a
tail (NonEmpty _ t) = t

cons :: a -> NonEmpty a -> NonEmpty a
cons x (NonEmpty y ys) = NonEmpty x (Cons y ys)

concat :: NonEmpty a -> NonEmpty a -> NonEmpty a
concat = flip (foldr cons)
