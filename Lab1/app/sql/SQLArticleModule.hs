module SQLArticleModule(addArticle, printArticles, printArticle, updateArticle, getArticle) where

import Database.SQLite.Simple

import ArticleModule(Article(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow Article where
   fromRow = Article <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

addArticle:: Article -> IO ()
addArticle article = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO articles (title, description, sectionId, authorId) VALUES (?, ?, ?, ?);"
                              (title article, description article, sectionId article, authorId article)

printArticleTable:: [Article] -> IO()
printArticleTable articles = do
                          putStr "Articles:\nid)\t|title\t|description\t|section id\t|author id\n"
                          mapM_ print articles

printArticles :: IO ()
printArticles = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM articles;" :: IO [Article]
               printArticleTable resp

getArticle:: Connection -> Int -> IO (Maybe Article)
getArticle conn articleId = do
                        resp <- query conn
                                "SELECT * FROM articles WHERE id = (?)"
                                (Only articleId) :: IO [Article]
                        return $ firstOrNothing resp

printArticle:: Int -> IO ()
printArticle articleId = withConn databaseName $
                            \conn -> do
                              resp <- getArticle conn articleId
                              case resp of
                                Just n  -> printArticleTable [n]
                                Nothing -> putStrLn "Not found!"

updateArticle:: Connection -> Article -> IO()
updateArticle conn article = do
                        findArticle <- getArticle conn (articleId article)
                        case findArticle of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE articles SET title = ?, description = ?,\
                                                       \ sectionId = ?, authorId = ? WHERE id = ?;"
                                                       (title article, description article, sectionId article, authorId article, articleId article) :: IO [Article]
                                        putStrLn "Updated!"
                           Nothing -> putStrLn "Not Found!"