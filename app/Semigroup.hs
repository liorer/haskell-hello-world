{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Semigroup where

import Prelude(Show, Num, Bool, (+), (*), (&&), (||))
import Function

class Semigroup a where
    (<>) :: a -> a -> a

newtype Sum a = Sum { getSum :: a } deriving Show

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

newtype Product a = Product { getProduct :: a } deriving Show

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

newtype Some = Some { getSome :: Bool } deriving Show

instance Semigroup Some where
    Some x <> Some y = Some (x || y)

newtype Every = Every { getEvery :: Bool } deriving Show

instance Semigroup Every where
    Every x <> Every y = Every (x && y)

newtype Dual a = Dual { getDual :: a } deriving Show

instance Semigroup a => Semigroup (Dual a) where
    Dual x <> Dual y = Dual (y <> x)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f • g)
