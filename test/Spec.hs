{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split as Split
import Data.Text
import Test.Hspec
import Test.QuickCheck
import NMEA0183

exampleGGA :: String
exampleGGA = "$GPGGA,013153.000,4518.7813,N,07555.3191,W,1,10,0.8,106.0,M,,,,0000*1B"

main :: IO ()
main = hspec $ do
    describe "NMEA0183" $ do
        it "parses GGA sentence" $ do
            let result = recordToNMEARecord (Split.splitOn "," exampleGGA)
            result `shouldBe` Right GGA {
                ggaSentence = pack exampleGGA,
                ggaTimeUTC = "013153.000",
                ggaLatitude = 4518.7813,
                ggaLongitude = (-7555.3191)
            }
