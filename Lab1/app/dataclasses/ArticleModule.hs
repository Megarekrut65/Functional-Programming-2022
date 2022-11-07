module ArticleModule(Article(..)) where

data Article = Article{articleId::Int
                      ,title::String
                      ,description::String
                      ,sectionId::Int
                      ,authorId::Int
                      }

instance Show Article where
   show article = mconcat [ show $ articleId article
                       , ".\t|"
                       , title article
                       , "\t|"
                       , description article
                       , "\t|"
                       , show $ sectionId article
                       , "\t|"
                       , show $ authorId article
                       ]