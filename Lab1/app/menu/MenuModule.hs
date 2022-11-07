module MenuModule(menu) where

import UserMenuModule(userMenu)
import AuthorMenuModule(authorMenu)
import SectionMenuModule(sectionMenu)
import ArticleMenuModule(articleMenu)
import StatisticMenuModule(statisticMenu)
import CommentMenuModule(commentMenu)

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
            3 -> do
                   sectionMenu
                   menu
            4 -> do
                   articleMenu
                   menu
            5 -> do
                   commentMenu
                   menu
            6 -> do
                   statisticMenu
                   menu
            0 -> return ()