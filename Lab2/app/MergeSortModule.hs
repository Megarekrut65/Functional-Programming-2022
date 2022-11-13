module MergeSortModule(parallelMergeSort, mergeSort) where

import Control.Monad.Par

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
             in merge (mergeSort ys) (mergeSort zs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

parallelMergeSort :: (Ord a, NFData a) => [a] -> [a]
parallelMergeSort xs | length xs <= 5000 = mergeSort xs
                 | otherwise           = runPar $ do
  let (ys, zs) = splitAt (length xs `div` 2) xs
  sys <- spawnP (parallelMergeSort ys)
  szs <- spawnP (parallelMergeSort zs)
  gys <- get sys
  gzs <- get szs
  return $ merge gys gzs