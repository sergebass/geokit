{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import NMEA0183
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  putStrLn ("Program name: " <> progName)
  args <- getArgs
  mapM_ processArgument args

processArgument :: String -> IO ()
processArgument arg = do
  putStrLn ("Processing file: " <> arg)
  processNMEAFile arg
