module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import SQLiteModule(withConn, executeCommands)
import UserModule(User(..))
import AuthorModule(Author(..))
import ArticleModule(Article(..))
import CommentModule(Comment(..))
import SectionModule(Section(..))
import StatisticModule(Statistic(..))
import SQLUserModule(addUser, printUsers, printUser, updateUser)
import ResourceModule(databaseName)

addArticle :: User -> IO ()
addArticle user = withConn databaseName $
                                    \conn -> do
                                      execute conn
                                        "INSERT INTO users (id, username, email) VALUES (?, ?, ?);"
                                        (UserModule.userId user, username user, email user)
createTables :: IO()
createTables = executeCommands ["CREATE TABLE users(id INTEGER PRIMARY KEY, username TEXT, email TEXT);",
                                       "CREATE TABLE authors(id INTEGER PRIMARY KEY, full_name TEXT, phone TEXT, position TEXT);",
                                       "CREATE TABLE sections(id INTEGER PRIMARY KEY, name TEXT);",
                                       "CREATE TABLE articles(id INTEGER PRIMARY KEY, title TEXT, description TEXT, sectionId INTEGER, authorId INTEGER, FOREIGN KEY(sectionId) REFERENCES sections(id), FOREIGN KEY(authorId) REFERENCES authors(id));",
                                       "CREATE TABLE comments(id INTEGER PRIMARY KEY, description TEXT, articleId INTEGER, userId INTEGER, FOREIGN KEY(articleId) REFERENCES articles(id), FOREIGN KEY(userId) REFERENCES users(id));",
                                       "CREATE TABLE statistics(id INTEGER PRIMARY KEY, view_count INTEGER, articleId INTEGER,FOREIGN KEY(articleId) REFERENCES articles(id));"
                                      ]

main :: IO ()
main = withConn databaseName $
                   \conn -> do printUser 1

