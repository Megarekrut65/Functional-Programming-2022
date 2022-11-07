module CommentMenuModule(commentMenu) where

import Database.SQLite.Simple

import CommentModule(Comment(..))
import SQLCommentModule(addComment, printComments, printComment, updateComment, getComment, printCommentsByArticle, printCommentsByUser)
import SQLiteModule(withConn)
import ResourceModule(databaseName)

addCommentMenu:: IO()
addCommentMenu = do
                putStrLn "Create new comment.\nEnter description:"
                description <- getLine
                putStrLn "Enter articleId:"
                articleIdLine <- getLine
                let articleId = (read articleIdLine::Int)
                putStrLn "Enter userId:"
                userIdLine <- getLine
                let userId = (read userIdLine::Int)
                addComment (Comment 0 description articleId userId)

updateCommentMenu:: IO()
updateCommentMenu = do
                putStrLn "Update existing comment.\nEnter id:"
                idLine <- getLine
                let commentId = (read idLine::Int)
                withConn databaseName $
                              \conn -> do
                                         resp <- getComment conn commentId
                                         case resp of
                                           Just n -> do
                                                        print $ n
                                                        putStrLn "Enter new description:"
                                                        description <- getLine
                                                        putStrLn "Enter articleId:"
                                                        articleIdLine <- getLine
                                                        let articleId = (read articleIdLine::Int)
                                                        putStrLn "Enter userId:"
                                                        userIdLine <- getLine
                                                        let userId = (read userIdLine::Int)
                                                        updateComment conn (Comment commentId description articleId userId)
                                           Nothing -> putStrLn "Not found!"

printCommentMenu:: IO()
printCommentMenu = do
                putStrLn "Print comment.\nEnter id:"
                idLine <- getLine
                let commentId = (read idLine::Int)
                printComment commentId

printArticleCommentsMenu:: IO()
printArticleCommentsMenu = do
                putStrLn "Print comment by articel.\nEnter article id:"
                idLine <- getLine
                let articleId = (read idLine::Int)
                printCommentsByArticle articleId

printUserCommentsMenu:: IO()
printUserCommentsMenu = do
                putStrLn "Print comment by user.\nEnter user id:"
                idLine <- getLine
                let userId = (read idLine::Int)
                printCommentsByUser userId

commentMenu:: IO()
commentMenu = do
              putStrLn "Select option:\n\
                        \1)Add new comment.\n2)Update existing comment.\n3)Print comment by id.\
                        \\n4)Print comments by article.\n5)Print comments by user.\n6)Print all comments.\n0)Back."
              line <- getLine
              let number = (read line ::Int)
              case number of
                1 -> do
                       addCommentMenu
                       commentMenu
                2 -> do
                       updateCommentMenu
                       commentMenu
                3 -> do
                       printCommentMenu
                       commentMenu
                4 -> do
                       printArticleCommentsMenu
                       commentMenu
                5 -> do
                       printUserCommentsMenu
                       commentMenu
                6 -> do
                       printComments
                       commentMenu
                0 -> return ()