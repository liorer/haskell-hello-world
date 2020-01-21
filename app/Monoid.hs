{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Monoid where

import Prelude (Num)
import Semigroup
import Foldable

class Semigroup a => Monoid a where
    mempty :: a

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Monoid (Product a) where
    mempty = Product 1

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty
