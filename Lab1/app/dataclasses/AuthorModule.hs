module AuthorModule(Author(..)) where

data Author = Author{authorId::Int
                    , fullName::String
                    , phone::String
                    , position::String
                    }

instance Show Author where
   show author = mconcat [ show $ authorId author
                       , ".\t|"
                       , fullName author
                       , "\t|"
                       , phone author
                       , "\t|"
                       , position author
                       ]
