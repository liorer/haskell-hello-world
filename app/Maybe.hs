{-# LANGUAGE NoImplicitPrelude #-}

module Maybe where

import Prelude (Show)

data Maybe a
    = Nothing
    | Just a
    deriving Show

(??) :: Maybe a -> a -> a
(??) Nothing f = f
(??) (Just a) _ = a
