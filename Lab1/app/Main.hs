module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Article = Article  { articleId::Int
                        , title::String
                        , description::String
                        }
instance Show Article where
   show article = mconcat [ show $ articleId article
                       , ".)  "
                       , title article
                       , " "
                       , description article
                       , "\n"]
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn
addArticle :: Int -> String -> String -> IO ()
addArticle articleId title description = withConn "FacultyNewspaper.db" $
                                    \conn -> do
                                      execute conn
                                        "INSERT INTO article (id, title, description) VALUES (?, ?, ?);"
                                        (articleId, title, description)
executeCommands :: [Query] -> IO()
executeCommands (command: commands) = do
                            withConn "FacultyNewspaper.db" $ \conn -> do execute conn command ()
                            executeCommands commands
createTables :: IO()
createTables = executeCommands ["CREATE TABLE users(id INTEGER PRIMARY KEY, username TEXT, email TEXT);",
                                       "CREATE TABLE authors(id INTEGER PRIMARY KEY, full_name TEXT, phone TEXT, position TEXT);",
                                       "CREATE TABLE articles(id INTEGER PRIMARY KEY, title TEXT, description TEXT, type TEXT,authorId INTEGER, FOREIGN KEY(authorId) REFERENCES authors(id));",
                                       "CREATE TABLE comments(id INTEGER PRIMARY KEY, description TEXT, articleId INTEGER, userId INTEGER, FOREIGN KEY(articleId) REFERENCES articles(id), FOREIGN KEY(userId) REFERENCES users(id));",
                                       "CREATE TABLE statistics(id INTEGER PRIMARY KEY, view_count INTEGER, articleId INTEGER,FOREIGN KEY(articleId) REFERENCES articles(id));"
                                      ]
main :: IO ()
main = putStrLn "Hello"

