module Main where

import SQLiteModule(withConn, executeCommands)
import MenuModule(menu)

createTables :: IO()
createTables = executeCommands ["CREATE TABLE users(id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, email TEXT);",
                                       "CREATE TABLE authors(id INTEGER PRIMARY KEY AUTOINCREMENT, full_name TEXT, phone TEXT, position TEXT);",
                                       "CREATE TABLE sections(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT);",
                                       "CREATE TABLE articles(id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, description TEXT, sectionId INTEGER, authorId INTEGER, FOREIGN KEY(sectionId) REFERENCES sections(id), FOREIGN KEY(authorId) REFERENCES authors(id));",
                                       "CREATE TABLE comments(id INTEGER PRIMARY KEY AUTOINCREMENT, description TEXT, articleId INTEGER, userId INTEGER, FOREIGN KEY(articleId) REFERENCES articles(id), FOREIGN KEY(userId) REFERENCES users(id));",
                                       "CREATE TABLE statistics(id INTEGER PRIMARY KEY AUTOINCREMENT, view_count INTEGER, articleId INTEGER,FOREIGN KEY(articleId) REFERENCES articles(id));"
                                      ]

main :: IO ()
main = menu

