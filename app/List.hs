{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module List where

import Prelude (Show(..), Int, Num, Bool(..), (+), (-), (*), (/), (++), (==), ($), (.), otherwise, undefined, concatMap, Eq)
import Foldable
import Semigroup
import Monoid
import Tuple
import Maybe

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

instance Semigroup (List a) where
    (<>) = concat

instance Monoid (List a) where
    mempty = Nil

nat :: Int -> List Int
nat 0 = Nil
nat n =
    let go m | m == n    = Cons m Nil
             | otherwise = Cons m (go(m + 1))
    in go 1

concat :: List a -> List a -> List a
concat = flip (foldr Cons)

concatMap :: (a -> List b) -> List a -> List b
concatMap f = foldr (concat ∘ f) Nil

map :: (a -> b) -> List a -> List b
map f = foldr (Cons ∘ f) Nil

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) f g x = f (g x)
infixr 9 ∘

(<|) :: a -> List a -> List a
(<|) = Cons
infixr 5 <|

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

reverse :: List a -> List a
reverse = foldl (flip Cons) Nil

take :: Int -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons h t) = Cons h (take (n - 1) t)

drop :: Int -> List a -> List a
drop 0 l = l
drop _ Nil = Nil
drop n (Cons _ t) = drop (n - 1) t

repeat :: a -> List a
repeat x = x <| (repeat x)

iterate :: (a -> a) -> a -> List a
iterate f x = x <| iterate f (f x)

cycle :: List a -> List a
cycle l = concat l (cycle l)

zip :: List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

unzip :: List (Tuple a b) -> Tuple (List a) (List b)
unzip = foldr (\(Tuple x y) (Tuple xs ys) -> Tuple (x <| xs) (y <| ys)) (Tuple Nil Nil)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil ys = Nil
zipWith f xs Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = (f x y) <| (zipWith f xs ys)

fromFoldable :: Foldable t => t a -> List a
fromFoldable = foldr Cons Nil

head :: List a -> Maybe a
head Nil = Nothing
head (Cons h _) = Just h

tail :: List a -> Maybe (List a)
tail Nil = Nothing
tail (Cons _ t) = Just t

nth :: Int -> List a -> Maybe a
nth _ Nil = Nothing
nth 0 l = head l
nth i l = nth (i - 1) t
    where
        t = case tail l of
                Nothing -> Nil
                Just x -> x

findIndex :: Eq a => a -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex x l = check x l 0
    where
        check _ Nil _ = Nothing
        check x (Cons h t) i | h == x    = Just i
                             | otherwise = check x t (i + 1)

sigma :: Num a => List a -> a
sigma = getSum . foldl (\acc v -> acc <> Sum v) mempty

pi :: Num a => List a -> a
pi = getProduct . foldl (\acc v -> acc <> Product v) mempty

