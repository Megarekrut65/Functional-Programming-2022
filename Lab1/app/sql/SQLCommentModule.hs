module SQLCommentModule(addComment, printComments, printComment, updateComment, getComment, printCommentsByArticle, printCommentsByUser) where

import Database.SQLite.Simple

import CommentModule(Comment(..))
import SQLiteModule(withConn, firstOrNothing)
import ResourceModule(databaseName)

instance FromRow Comment where
   fromRow = Comment <$> field
                     <*> field
                     <*> field
                     <*> field

addComment:: Comment -> IO ()
addComment comment = do
                  withConn databaseName $
                          \conn -> do
                            execute conn
                              "INSERT INTO comments (description, articleId, userId) VALUES (?, ?, ?);"
                              (description comment, articleId comment, userId comment)

printCommentTable:: [Comment] -> IO()
printCommentTable comments = do
                          putStr "Comments:\nid)\t|description\t|article id\t|user id\n"
                          mapM_ print comments

printComments :: IO ()
printComments = withConn databaseName $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM comments;" :: IO [Comment]
               printCommentTable resp

getComment:: Connection -> Int -> IO (Maybe Comment)
getComment conn commentId = do
                        resp <- query conn
                                "SELECT * FROM comments WHERE id = (?)"
                                (Only commentId) :: IO [Comment]
                        return $ firstOrNothing resp

printCommentsByArticle :: Int -> IO ()
printCommentsByArticle articleId = withConn databaseName $
             \conn ->  do
               resp <- query conn "SELECT * FROM comments WHERE articleId = ?;"
                                  (Only articleId) :: IO [Comment]
               printCommentTable resp

printCommentsByUser :: Int -> IO ()
printCommentsByUser userId = withConn databaseName $
             \conn ->  do
               resp <- query conn "SELECT * FROM comments WHERE userId = ?;"
                                  (Only userId) :: IO [Comment]
               printCommentTable resp

printComment:: Int -> IO ()
printComment commentId = withConn databaseName $
                            \conn -> do
                              resp <- getComment conn commentId
                              case resp of
                                Just n  -> printCommentTable [n]
                                Nothing -> putStrLn "Not found!"

updateComment:: Connection -> Comment -> IO()
updateComment conn comment = do
                        findComment <- getComment conn (commentId comment)
                        case findComment of
                           Just n  -> do
                                        resp <- query conn
                                                       "UPDATE comments SET description = ?,\
                                                       \ articleId = ?, userId = ? WHERE id = ?;"
                                                       (description comment, articleId comment, userId comment, commentId comment) :: IO [Comment]
                                        putStrLn "Updated!"
                           Nothing -> putStrLn "Not Found!"