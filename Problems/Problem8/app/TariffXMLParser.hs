module TariffXMLParser(run) where

import Text.XML.HXT.Core
import TariffModule(Tariff(..))

run :: String -> IO ()
run file = do
            input <- readFile file
            names <- runX $ readString [withValidate no] input
                    //> hasName "Name"
                    //> getText
            operators <- runX $ readString [withValidate no] input
                    //> hasName "Operator"
                    //> getText
            payrolls <- runX $ readString [withValidate no] input
                    //> hasName "Payroll"
                    //> getText
            call_price <- runX $ readString [withValidate no] input
                    //> hasName "CallPrice"
                    //> getText
            sms_price <- runX $ readString [withValidate no] input
                    //> hasName "SMSPrice"
                    //> getText
            print names
            print operators
            print payrolls
            print call_price
            print sms_price
            putStrLn "Parsed"