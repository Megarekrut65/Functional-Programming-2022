module ArticleMenuModule(articleMenu) where

import Database.SQLite.Simple

import ArticleModule(Article(..))
import SQLArticleModule(printArticles, printArticle, updateArticle, addArticle, getArticle)
import SQLStatisticModule(addStatistic, updateStatistic, getStatistic, getStatisticByArticle)
import StatisticModule(Statistic(..))
import SQLiteModule(withConn)
import ResourceModule(databaseName)

addArticleMenu:: IO()
addArticleMenu = do
                putStrLn "Create new article.\nEnter title:"
                title <- getLine
                putStrLn "Enter description:"
                description <- getLine
                putStrLn "Enter sectionId:"
                sectionIdLine <- getLine
                let sectionId = (read sectionIdLine::Int)
                putStrLn "Enter authorId:"
                authorIdLine <- getLine
                let authorId = (read authorIdLine::Int)
                addArticle (Article 0 title description sectionId authorId)

updateArticleMenu:: IO()
updateArticleMenu = do
                putStrLn "Update existing article.\nEnter id:"
                idLine <- getLine
                let articleId = (read idLine::Int)
                withConn databaseName $
                              \conn -> do
                                         resp <- getArticle conn articleId
                                         case resp of
                                           Just n -> do
                                                        print $ n
                                                        putStrLn "Enter new title:"
                                                        title <- getLine
                                                        putStrLn "Enter new description:"
                                                        description <- getLine
                                                        putStrLn "Enter new sectionId:"
                                                        sectionIdLine <- getLine
                                                        let sectionId = (read sectionIdLine::Int)
                                                        putStrLn "Enter new authorId:"
                                                        authorIdLine <- getLine
                                                        let authorId = (read authorIdLine::Int)
                                                        updateArticle conn (Article articleId title description sectionId authorId)
                                           Nothing -> putStrLn "Not found!"

printArticleMenu:: IO()
printArticleMenu = do
                putStrLn "Print article.\nEnter id:"
                idLine <- getLine
                let articleId = (read idLine::Int)
                printArticle articleId
                withConn databaseName $
                           \conn -> do
                                      statistic <- getStatisticByArticle conn articleId
                                      case statistic of
                                        Just n -> do
                                                    updateStatistic conn (Statistic (statisticId n) (1 + viewCount n) articleId)
                                        Nothing -> do
                                                     addStatistic (Statistic 0 1 articleId)

articleMenu:: IO()
articleMenu = do
              putStrLn "Select option:\n\
                        \1)Add new article.\n2)Update existing article.\n3)Print article by id.\n4)Print all articles.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       addArticleMenu
                       articleMenu
                2 -> do
                       updateArticleMenu
                       articleMenu
                3 -> do
                       printArticleMenu
                       articleMenu
                4 -> do
                       printArticles
                       articleMenu
                0 -> return ()