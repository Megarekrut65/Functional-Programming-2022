module SQLSectionModule(addSection, printSections, printSection, updateSection, getSection) where

import Database.SQLite.Simple
import SectionModule(Section(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow Section where
   fromRow = Section <$> field
                     <*> field

addSection:: Section -> IO ()
addSection section = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO sections (name) VALUES (?);"
                              (Only (name section))

printSectionTable:: [Section] -> IO()
printSectionTable sections = do
                          putStr "Sections:\nid)\t|name\n"
                          mapM_ print sections

printSections :: IO ()
printSections = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM sections;" :: IO [Section]
               printSectionTable resp

getSection:: Connection -> Int -> IO (Maybe Section)
getSection conn sectionId = do
                        resp <- query conn
                                "SELECT * FROM sections WHERE id = (?)"
                                (Only sectionId) :: IO [Section]
                        return $ firstOrNothing resp

printSection:: Int -> IO ()
printSection sectionId = withConn databaseName $
                            \conn -> do
                              resp <- getSection conn sectionId
                              case resp of
                                Just n  -> printSectionTable [n]
                                Nothing -> putStrLn "Not found!"

updateSection:: Connection -> Section -> IO()
updateSection conn section = do
                        findSection <- getSection conn (sectionId section)
                        case findSection of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE sections SET name = ? WHERE id = ?;"
                                                       (name section, sectionId section) :: IO [Section]
                                        putStrLn "Updated!"
                           Nothing -> putStrLn "Not Found!"