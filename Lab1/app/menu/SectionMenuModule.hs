module SectionMenuModule(sectionMenu) where

import SectionModule(Section(..))
import SQLSectionModule(printSections, printSection, updateSection, addSection, getSection)
import SQLiteModule(withConn)
import ResourceModule(databaseName)

addSectionMenu:: IO()
addSectionMenu = do
                putStrLn "Create new section.\nEnter name:"
                name <- getLine
                addSection (Section 0 name)
                putStrLn "Added!"

updateSectionMenu:: IO()
updateSectionMenu = do
                putStrLn "Update existing section.\nEnter id:"
                idLine <- getLine
                let sectionId = (read idLine::Int)
                withConn databaseName $
                              \conn -> do
                                         resp <- getSection conn sectionId
                                         case resp of
                                           Just n  -> do
                                                        print $ n
                                                        putStrLn "Enter new name:"
                                                        name <- getLine
                                                        updateSection conn (Section sectionId name)
                                           Nothing -> putStrLn "Not found!"

printSectionMenu:: IO()
printSectionMenu = do
                putStrLn "Print section.\nEnter id:"
                idLine <- getLine
                let sectionId = (read idLine::Int)
                printSection sectionId

sectionMenu:: IO()
sectionMenu = do
              putStrLn "Select option:\n\
                        \1)Add new section.\n2)Update existing section.\n3)Print section by id.\n4)Print all sections.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       addSectionMenu
                       sectionMenu
                2 -> do
                       updateSectionMenu
                       sectionMenu
                3 -> do
                       printSectionMenu
                       sectionMenu
                4 -> do
                       printSections
                       sectionMenu
                0 -> return ()