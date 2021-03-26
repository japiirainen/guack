module Main where

import           Lib (fib)

main :: IO ()
main =
    print $ fib 10
