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
main :: IO ()
main = do addArticle 3 "Hello" "World"