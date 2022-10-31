module StatisticModule(Statistic(..)) where

data Statistic = Statistic{statisticId::Int
                          ,view_count::Int
                          ,articleId::Int
                          }

instance Show Statistic where
   show statistic = mconcat [ show $ statisticId statistic
                       , ")\t"
                       , show $ view_count statistic
                       , "\t"
                       , show $ articleId statistic
                       , "\n"]