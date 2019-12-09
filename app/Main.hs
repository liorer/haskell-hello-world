module Main where

import Prelude hiding (zipWith, take, drop)
import List

main :: IO ()
main = pure ()

fib :: List Int
fib = zipWith (+) (0 <| 1 <| fib) (0 <| fib)
