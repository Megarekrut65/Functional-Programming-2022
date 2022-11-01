module MenuModule(menu) where

import UserMenuModule(userMenu)
import AuthorMenuModule(authorMenu)

menu:: IO()
menu = do
          putStrLn "Menu:\n1)Users.\n2)Authors\n3)Sections.\n4)Articles.\n5)Comments.\n6)Statistic.\n0)Exit."
          line <- getLine
          let number = (read line ::Int)
          case number of
            1 -> do
                   userMenu
                   menu
            2 -> do
                   authorMenu
                   menu
            0 -> return ()