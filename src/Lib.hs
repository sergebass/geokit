{-# LANGUAGE OverloadedStrings #-}

module Lib
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
  mapM_ processRecord csv
  -- mapM_ (putStrLn . show) csv

processRecord :: Record -> IO ()
processRecord r = do
  let recordType : fields = r
  putStrLn $ recordType <> " => " <> show fields

-- type Latitude = Float
-- type Longitude = Float
-- type Altitude = Float

-- data Point = Point
--   { location :: Maybe (Latitude, Longitude)
--   , altitude :: Maybe Altitude
--   } deriving Show

-- mergePoints :: Point -> Point -> Point
-- mergePoints p1 p2 = p1  -- FIXME tmp!
