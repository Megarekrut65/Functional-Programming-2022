module Main where

import TariffModule(Tariff(..))

main :: IO ()
main = print (Tariff "Super+" "MTS" 100 10 5)