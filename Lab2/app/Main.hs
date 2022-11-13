module Main where

import System.IO
import Data.Time

import MergeSortModule(parallelMergeSort, mergeSort)

main :: IO ()
main = do
            begT <- getCurrentTime
            let res = (mergeSort [1..10000001] :: [Int])
            print (elem 10000000 res)
            endT <- getCurrentTime
            putStrLn $ init $ show $ diffUTCTime endT begT
            begT <- getCurrentTime
            let res2 = (parallelMergeSort [1..10000001] :: [Int])
            print (elem 10000000 res2)
            endT <- getCurrentTime
            putStrLn $ init $ show $ diffUTCTime endT begT
