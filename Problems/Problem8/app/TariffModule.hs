module TariffModule(Tariff(..)) where

data Tariff = Tariff{name::String
                      ,operator::String
                      ,payroll::Int
                      ,call_price::Int
                      ,sms_price::Int
                      }

instance Show Tariff where
   show tariff = mconcat [ name tariff
                       , ", "
                       , operator tariff
                       , ", "
                       , show $ payroll tariff
                       , ", "
                       , show $ call_price tariff
                       , ", "
                       , show $ sms_price tariff
                       ]