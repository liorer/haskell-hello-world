{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Semigroup where

import Prelude(Show, Num, (+), (*))

class Semigroup a where
    (<>) :: a -> a -> a

newtype Sum a = Sum { getSum :: a } deriving Show

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

newtype Product a = Product { getProduct :: a } deriving Show

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

newtype Dual a = Dual a deriving Show

instance Semigroup a => Semigroup (Dual a) where
    Dual x <> Dual y = Dual (y <> x)
