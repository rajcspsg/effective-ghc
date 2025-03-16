module InfiniteFind
  ( findFirst,
  )
where

findFirst :: (a -> Bool) -> [a] -> [a]
findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound
