module SQLiteModule(withConn, executeCommands, firstOrNothing) where

import Control.Applicative
import Database.SQLite.Simple
import ResourceModule(databaseName)

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
     conn <- open dbName
     action conn
     close conn

executeCommands :: [Query] -> IO()
executeCommands (command: commands) = do
                              withConn databaseName $ \conn -> do execute conn command ()
                              executeCommands commands
firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x
