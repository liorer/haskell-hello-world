{-# LANGUAGE InstanceSigs #-}

module List where

import Prelude hiding (foldl, foldr, map, sum, flip, take, drop, Foldable)
import Foldable (Foldable(..))

data List a
    = Nil
    | Cons a (List a)

instance Show a => Show (List a) where
    show l =
        "[" ++ (sprint l) ++ "]"
            where
                sprint Nil = ""
                sprint (Cons h Nil) = show h
                sprint (Cons h t) = (show h) ++ ", " ++ (sprint t)

instance Foldable List where
    foldl :: (b -> a -> b) -> b -> List a -> b
    foldl _ z Nil = z
    foldl f z (Cons h t) = foldl f (f z h) t

    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr _ z Nil = z
    foldr f z (Cons h t) = f h (foldr f z t)

nat :: Int -> List Int
nat 0 = Nil
nat n =
    go 1
        where
            go m = if m == n then Cons m Nil else Cons m (go(m + 1))


map :: (a -> b) -> List a -> List b
map f = foldr (\x l -> Cons (f x) l) Nil

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

take :: Int -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons h t) = Cons h (take (n - 1) t)

--drop :: Int -> List a -> List a
drop 0 l = l
drop _ Nil = Nil
drop n (Cons _ t) = drop (n - 1) t
