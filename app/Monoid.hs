{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Monoid where

import Prelude (Num, Bool(..), (||), (&&), undefined, id)
import Semigroup
import Function
import Foldable

class Semigroup a => Monoid a where
    mempty :: a

foldMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap f = foldl (\acc v -> acc <> f v) mempty

fold :: (Foldable f, Monoid m) => f m -> m
fold = foldMap id

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Monoid (Product a) where
    mempty = Product 1

instance Monoid Some where
    mempty = Some False

instance Monoid Every where
    mempty = Every True

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty

instance Monoid (Endo a) where
    mempty = Endo id

type Predicate a = a -> Bool

some :: Foldable f => Predicate a -> f a -> Bool
some p = getSome • foldMap (Some • p)

every :: Foldable f => Predicate a -> f a -> Bool
every p = getEvery • foldMap (Every • p)
