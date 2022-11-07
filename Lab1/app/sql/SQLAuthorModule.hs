module SQLAuthorModule(addAuthor, printAuthors, printAuthor, updateAuthor, getAuthor) where

import Database.SQLite.Simple
import AuthorModule(Author(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow Author where
   fromRow = Author <$> field
                    <*> field
                    <*> field
                    <*> field

addAuthor:: Author -> IO ()
addAuthor author = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO authors (full_name, phone, position) VALUES (?, ?, ?);"
                              (fullName author, phone author, position author)

printAuthorTable:: [Author] -> IO()
printAuthorTable authors = do
                          putStr "Authors:\nid)\t|full name\t|phone\t|position\n"
                          mapM_ print authors

printAuthors :: IO ()
printAuthors = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM authors;" :: IO [Author]
               printAuthorTable resp

getAuthor:: Connection -> Int -> IO (Maybe Author)
getAuthor conn authorId = do
                        resp <- query conn
                                "SELECT * FROM authors WHERE id = (?)"
                                (Only authorId) :: IO [Author]
                        return $ firstOrNothing resp

printAuthor:: Int -> IO ()
printAuthor authorId = withConn databaseName $
                            \conn -> do
                              resp <- getAuthor conn authorId
                              case resp of
                                Just n  -> printAuthorTable [n]
                                Nothing -> putStrLn "Not found!"

updateAuthor:: Connection -> Author -> IO()
updateAuthor conn author = do
                        findAuthor <- getAuthor conn (authorId author)
                        case findAuthor of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE authors SET full_name = ?, phone = ?, position = ? WHERE id = ?;"
                                                       (fullName author, phone author, position author, authorId author) :: IO [Author]
                                        putStrLn "Updated!"
                           Nothing -> putStrLn "Not Found!"