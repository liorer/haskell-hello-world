{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Function where

(•) :: (b -> c) -> (a -> b) -> a -> c
(•) f g x = f (g x)
infixr 9 •
