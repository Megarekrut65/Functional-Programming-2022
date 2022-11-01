module UserMenuModule(userMenu) where

import UserModule(User(..))
import SQLUserModule(printUsers, printUser, updateUser, addUser, getUser)
import SQLiteModule(withConn)
import ResourceModule(databaseName)

addUserMenu:: IO()
addUserMenu = do
                putStrLn "Create new user.\nEnter username:"
                username <- getLine
                putStrLn "Enter email:"
                email <- getLine
                addUser (User 0 username email)
                putStrLn "Added!"

updateUserMenu:: IO()
updateUserMenu = do
                putStrLn "Update existing user.\nEnter id:"
                idLine <- getLine
                let userId = (read idLine::Int)
                withConn databaseName $
                              \conn -> do
                                         resp <- getUser conn userId
                                         case resp of
                                           Just n  -> do
                                                        print $ n
                                                        putStrLn "Enter new username:"
                                                        username <- getLine
                                                        putStrLn "Enter new email:"
                                                        email <- getLine
                                                        updateUser conn (User userId username email)
                                           Nothing -> putStrLn "Not found!"

printUserMenu:: IO()
printUserMenu = do
                putStrLn "Print user.\nEnter id:"
                idLine <- getLine
                let userId = (read idLine::Int)
                printUser userId

userMenu:: IO()
userMenu = do
              putStrLn "Select option:\n\
                        \1)Add new user.\n2)Update existing user.\n3)Print user by id.\n4)Print all users.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       addUserMenu
                       userMenu
                2 -> do
                       updateUserMenu
                       userMenu
                3 -> do
                       printUserMenu
                       userMenu
                4 -> do
                       printUsers
                       userMenu
                0 -> return ()