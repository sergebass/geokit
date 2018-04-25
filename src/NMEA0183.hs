{-# LANGUAGE OverloadedStrings #-}

module NMEA0183
    ( processNMEAFile
    ) where

import Data.Monoid
import qualified Data.Text as T
import Text.CSV
import Text.Parsec.Error

processNMEAFile :: FilePath -> IO ()
processNMEAFile fileName = do
    putStrLn ("Parsing CSV from " <> fileName)
    handleParsingResult $ parseCSVFromFile fileName

handleParsingResult :: IO (Either ParseError CSV) -> IO ()
handleParsingResult resultM = do
  result <- resultM
  case result of
    Left error -> putStrLn "OOPS!"
    Right csv -> transformCSV csv
    -- Right csv -> putStrLn "Yahoo!"

transformCSV :: CSV -> IO ()
transformCSV csv = do
  mapM_ (putStrLn . show) (csvToNMEARecords csv)

data NMEARecord = GPGGA [String]
                | GPRMC [String]
                deriving Show

csvToNMEARecords :: CSV -> [NMEARecord]
csvToNMEARecords csv = map recordToNMEARecord csv

recordToNMEARecord :: Record -> NMEARecord
recordToNMEARecord (recordType : fields) =
    case recordType of
        "$GPGGA" -> GPGGA fields
        "$GPRMC" -> GPRMC fields
        otherwise -> GPGGA [] -- FIXME

type Latitude = Float
type Longitude = Float
type Altitude = Float

data Point = Point
    { location :: Maybe (Latitude, Longitude)
    , altitude :: Maybe Altitude
    } deriving Show

csvToPoints :: CSV -> [Point]
csvToPoints csv = undefined
