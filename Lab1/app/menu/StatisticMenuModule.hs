module StatisticMenuModule(statisticMenu) where

import StatisticModule(Statistic(..))
import SQLStatisticModule(printStatistics, printStatistic, getStatisticByArticle, printStatisticByArticle)
import SQLiteModule(withConn)
import ResourceModule(databaseName)

printStatisticMenu:: IO()
printStatisticMenu = do
                putStrLn "Print statistic.\nEnter article id:"
                idLine <- getLine
                let articleId = (read idLine::Int)
                printStatisticByArticle articleId

statisticMenu:: IO()
statisticMenu = do
              putStrLn "Select option:\n\
                        \1)Print statistic by article id.\n2)Print all statistics.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       printStatisticMenu
                       statisticMenu
                2 -> do
                       printStatistics
                       statisticMenu
                0 -> return ()