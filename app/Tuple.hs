{-# LANGUAGE NoImplicitPrelude #-}

module Tuple where

import Prelude (Show(..), (++), (+), (-))

data Tuple a b =
    Tuple a b

instance (Show a, Show b) => Show (Tuple a b) where
    show (Tuple a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
