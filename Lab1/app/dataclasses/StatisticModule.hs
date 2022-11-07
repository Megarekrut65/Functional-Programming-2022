module StatisticModule(Statistic(..)) where

data Statistic = Statistic{statisticId::Int
                          ,viewCount::Int
                          ,articleId::Int
                          }

instance Show Statistic where
   show statistic = mconcat [ show $ statisticId statistic
                       , ".\t|"
                       , show $ viewCount statistic
                       , "\t|"
                       , show $ articleId statistic
                       ]