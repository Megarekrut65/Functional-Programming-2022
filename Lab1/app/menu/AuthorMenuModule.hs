module AuthorMenuModule(authorMenu) where

import AuthorModule(Author(..))
import SQLAuthorModule(printAuthors, printAuthor, updateAuthor, addAuthor, getAuthor)
import SQLiteModule(withConn)
import ResourceModule(databaseName)

addAuthorMenu:: IO()
addAuthorMenu = do
                putStrLn "Create new author.\nEnter full name:"
                fullName <- getLine
                putStrLn "Enter phone:"
                phone <- getLine
                putStrLn "Enter position:"
                position <- getLine
                addAuthor (Author 0 fullName phone position)
                putStrLn "Added!"

updateAuthorMenu:: IO()
updateAuthorMenu = do
                putStrLn "Update existing author.\nEnter id:"
                idLine <- getLine
                let authorId = (read idLine::Int)
                withConn databaseName $
                              \conn -> do
                                         resp <- getAuthor conn authorId
                                         case resp of
                                           Just n  -> do
                                                        print $ n
                                                        putStrLn "Enter new full name:"
                                                        fullName <- getLine
                                                        putStrLn "Enter new phone:"
                                                        phone <- getLine
                                                        putStrLn "Enter new position:"
                                                        position <- getLine
                                                        updateAuthor conn (Author authorId fullName phone position)
                                           Nothing -> putStrLn "Not found!"

printAuthorMenu:: IO()
printAuthorMenu = do
                putStrLn "Print author.\nEnter id:"
                idLine <- getLine
                let authorId = (read idLine::Int)
                printAuthor authorId

authorMenu:: IO()
authorMenu = do
              putStrLn "Select option:\n\
                        \1)Add new author.\n2)Update existing author.\n3)Print author by id.\n4)Print all authors.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       addAuthorMenu
                       authorMenu
                2 -> do
                       updateAuthorMenu
                       authorMenu
                3 -> do
                       printAuthorMenu
                       authorMenu
                4 -> do
                       printAuthors
                       authorMenu
                0 -> return ()