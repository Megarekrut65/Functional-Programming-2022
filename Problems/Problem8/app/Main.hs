module Main where

import TariffModule(Tariff(..))
import TariffXMLParser(run)


main:: IO()
main = run "input.xml"