{-# LANGUAGE TupleSections #-}

module Transforming
  ( doubleElems,
    pairs,
  )
where

import Prelude

doubleElems :: [Int] -> [Int]
doubleElems = foldr doubleElem []
  where
    doubleElem num lst = (2 * num) : lst

pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      makePairs a = map (a,) bs'
   in concatMap makePairs as'
