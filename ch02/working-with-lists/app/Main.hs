module Main (main) where

import InfiniteFind (findFirst)
import Lib
import Transforming (pairs)

add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main =
  -- print $ pairs [1 .. 10] [2 .. 5]
  print $ findFirst (> 5) [1 .. 100]
