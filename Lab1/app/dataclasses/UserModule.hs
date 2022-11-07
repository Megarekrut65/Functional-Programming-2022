module UserModule(User(..)) where

data User = User { userId::Int
                 , username::String
                 , email::String
                 }
instance Show User where
   show user = mconcat [ show $ userId user
                       , ".\t|"
                       , username user
                       , "\t|"
                       , email user
                       ]