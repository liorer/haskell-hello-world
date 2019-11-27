{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module BinaryTree where

import Prelude (Show, Int)
import List (List(..))
import Foldable (Foldable(..))

data BinaryTree a
    = Empty
    | Leaf a
    | Node a (BinaryTree a) (BinaryTree a)
    deriving Show

--sum :: BinaryTree Int -> Int
--sum Empty = 0
--sum (Leaf x) = x
--sum (Node x l r) = x + sum l + sum r

instance Foldable BinaryTree where
    foldl :: (b -> a -> b) -> b -> BinaryTree a -> b
    foldl _ z Empty = z
    foldl f z (Leaf x) = f z x
    foldl f z (Node x l r) = foldl f (f (foldl f z l) x) r

    foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldr _ z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l

toList :: BinaryTree a -> List a
toList t = foldr Cons Nil t

t :: BinaryTree Int
t = Node 4 (Leaf 7) (Node 11 Empty (Leaf 12))
