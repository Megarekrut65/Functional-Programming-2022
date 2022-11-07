module SQLUserModule(addUser, printUsers, printUser, updateUser, getUser) where

import Database.SQLite.Simple
import UserModule(User(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow User where
   fromRow = User <$> field
                  <*> field
                  <*> field

addUser:: User -> IO ()
addUser user = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO users (username, email) VALUES (?, ?);"
                              (username user, email user)

printUserTable:: [User] -> IO()
printUserTable users = do
                          putStr "Users:\nid)\t|username\t|email\n"
                          mapM_ print users

printUsers :: IO ()
printUsers = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM users;" :: IO [User]
               printUserTable resp

getUser:: Connection -> Int -> IO (Maybe User)
getUser conn userId = do
                        resp <- query conn
                                "SELECT * FROM users WHERE id = (?)"
                                (Only userId) :: IO [User]
                        return $ firstOrNothing resp

printUser:: Int -> IO ()
printUser userId = withConn databaseName $
                            \conn -> do
                              resp <- getUser conn userId
                              case resp of
                                Just n  -> printUserTable [n]
                                Nothing -> putStrLn "Not found!"

updateUser:: Connection -> User -> IO()
updateUser conn user = do
                        findUser <- getUser conn (userId user)
                        case findUser of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE users SET username = ?, email = ? WHERE id = ?;"
                                                       (username user, email user, userId user) :: IO [User]
                                        putStrLn "Updated!"
                           Nothing -> putStrLn "Not Found!"