module SQLStatisticModule(addStatistic, printStatistics, printStatistic, updateStatistic, getStatistic, getStatisticByArticle, printStatisticByArticle) where

import Database.SQLite.Simple
import StatisticModule(Statistic(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow Statistic where
   fromRow = Statistic <$> field
                       <*> field
                       <*> field

addStatistic:: Statistic -> IO ()
addStatistic statistic = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO statistics (view_count, articleId) VALUES (?, ?);"
                              (viewCount statistic, articleId statistic)

printStatisticTable:: [Statistic] -> IO()
printStatisticTable statistics = do
                          putStr "Statistics:\nid)\t|view count\t|article id\n"
                          mapM_ print statistics

printStatistics :: IO ()
printStatistics = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM statistics;" :: IO [Statistic]
               printStatisticTable resp

getStatistic:: Connection -> Int -> IO (Maybe Statistic)
getStatistic conn statisticId = do
                        resp <- query conn
                                "SELECT * FROM statistics WHERE id = (?)"
                                (Only statisticId) :: IO [Statistic]
                        return $ firstOrNothing resp

getStatisticByArticle:: Connection -> Int -> IO (Maybe Statistic)
getStatisticByArticle conn articleId = do
                        resp <- query conn
                                "SELECT * FROM statistics WHERE articleId = (?)"
                                (Only articleId) :: IO [Statistic]
                        return $ firstOrNothing resp

printStatisticByArticle:: Int -> IO ()
printStatisticByArticle articleId = withConn databaseName $
                            \conn -> do
                              resp <- getStatisticByArticle conn articleId
                              case resp of
                                Just n  -> printStatisticTable [n]
                                Nothing -> putStrLn "Not found!"

printStatistic:: Int -> IO ()
printStatistic statisticId = withConn databaseName $
                            \conn -> do
                              resp <- getStatistic conn statisticId
                              case resp of
                                Just n  -> printStatisticTable [n]
                                Nothing -> putStrLn "Not found!"

updateStatistic:: Connection -> Statistic -> IO()
updateStatistic conn statistic = do
                        findStatistic <- getStatistic conn (statisticId statistic)
                        case findStatistic of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE statistics SET view_count = ?, articleId = ? WHERE id = ?;"
                                                       (viewCount statistic, articleId statistic, statisticId statistic) :: IO [Statistic]
                                        return ()
                           Nothing -> putStrLn "Not Found!"