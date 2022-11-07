module CommentModule(Comment(..)) where

data Comment = Comment{commentId::Int
                      ,description::String
                      ,articleId::Int
                      ,userId::Int
                      }

instance Show Comment where
   show comment = mconcat [ show $ commentId comment
                       , ".\t|"
                       , description comment
                       , "\t|"
                       , show $ articleId comment
                       , "\t|"
                       , show $ userId comment
                       ]