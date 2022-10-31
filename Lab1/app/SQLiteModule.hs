module SQLiteModule(withConn, executeCommands) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
     conn <- open dbName
     action conn
     close conn

executeCommands :: [Query] -> IO()
executeCommands (command: commands) = do
                              withConn "FacultyNewspaper.db" $ \conn -> do execute conn command ()
                              executeCommands commands