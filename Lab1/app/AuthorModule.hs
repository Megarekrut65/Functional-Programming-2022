module AuthorModule(Author(..)) where

data Author = Author{authorId::Int
                    , full_name::String
                    , phone::String
                    , position::String
                    }

instance Show Author where
   show author = mconcat [ show $ authorId author
                       , ")\t"
                       , full_name author
                       , "\t"
                       , phone author
                       , "\t"
                       , position author
                       , "\n"]
