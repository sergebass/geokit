{-# LANGUAGE OverloadedStrings #-}

module NMEA0183
    ( processNMEAFile
    ) where

import Data.List
import Data.Monoid
import Data.Text (Text, pack)
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
    Left error -> putStrLn ("Unable to parse the NMEA track log:" <> show error)
    Right csv -> transformCSV csv

transformCSV :: CSV -> IO ()
transformCSV csv = do
  mapM_ (putStrLn . show) (csvToNMEARecords csv)

data NMEARecord  -- FIXME call it Sentence
  = GGA  -- Global Positioning System Fix Data
      { ggaSentence :: Text
      , ggaTimeUTC :: Text
      , ggaLatitude :: Double
      , ggaLongitude :: Double
      -- FIXME add remaining fields
      }
  | GSA [Field]  -- GPS DOP and active satellites
  | GSV [Field]  -- Satellites in view
  | RMC [Field]  -- Recommended Minimum Navigation Information
  | VTG [Field]  -- Track Made Good and Ground Speed
  deriving Show

type ParsingError = Text

csvToNMEARecords :: CSV -> [Either ParsingError NMEARecord]
csvToNMEARecords csv = map recordToNMEARecord csv

recordToNMEARecord :: Record -> Either ParsingError NMEARecord
recordToNMEARecord (recordType : fields) =
    case recordType of
        "$GPGGA" -> decodeGGA fields
        "$GPGSA" -> decodeGSA fields
        "$GPGSV" -> decodeGSV fields
        "$GPRMC" -> decodeRMC fields
        "$GPVTG" -> decodeVTG fields
        otherwise -> Left ("Unknown NMEA sentence type: " <> pack recordType)

restoreSentenceText :: [Field] -> Text
restoreSentenceText sentence = pack $ intercalate "," sentence

{----------------------------------------------------------------------------
GGA: Global Positioning System Fix Data. Time, Position and fix related data
for a GPS receiver

       1         2       3 4        5 6 7  8   9  10 11 12 13  14   15
       |         |       | |        | | |  |   |   | |   | |   |    |
$--GGA,hhmmss.ss,llll.ll,a,yyyyy.yy,a,x,xx,x.x,x.x,M,x.x,M,x.x,xxxx*hh
 1) Time (UTC)
 2) Latitude
 3) N or S (North or South)
 4) Longitude
 5) E or W (East or West)
 6) GPS Quality Indicator,
    0 - fix not available,
    1 - GPS fix,
    2 - Differential GPS fix
 7) Number of satellites in view, 00 - 12
 8) Horizontal Dilution of precision
 9) Antenna Altitude above/below mean-sea-level (geoid)
10) Units of antenna altitude, meters
11) Geoidal separation, the difference between the WGS-84 earth ellipsoid and mean-sea-level (geoid), "-" means mean-sea-level below ellipsoid
12) Units of geoidal separation, meters
13) Age of differential GPS data, time in seconds since last SC104 type 1 or 9 update, null field when DGPS is not used
14) Differential reference station ID, 0000-1023
15) Checksum
-----------------------------------------------------------------------------}
decodeGGA :: [Field] -> Either ParsingError NMEARecord
decodeGGA sentence@(timeString
                  : latitudeString
                  : northOrSouthString
                  : longitudeString
                  : eastOrWestString
                  : _)
  = Right $ GGA { ggaSentence = restoreSentenceText sentence
                , ggaTimeUTC = pack timeString
                , ggaLatitude = let latitude = read latitudeString
                                in if northOrSouthString == "N"  -- Northern hemisphere
                                    then latitude
                                    else (- latitude)  -- "Down Under"
                , ggaLongitude = let longitude = read longitudeString
                                 in if eastOrWestString == "E"  -- Eastern hemisphere
                                     then longitude
                                     else (- longitude)  -- Western hemisphere
                }
decodeGGA sentence = Left $ "Cannot parse GGA sentence: " <> restoreSentenceText sentence

{----------------------------------------------------------------------------
GSA: GPS DOP and active satellites

       1 2 3                         14 15  16  17 18
       | | |                         |  |   |   |  |
$--GSA,a,a,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x.x,x.x,x.x*hh
 1) Selection mode
 2) Mode
 3) ID of 1st satellite used for fix
 4) ID of 2nd satellite used for fix
 ...
14) ID of 12th satellite used for fix
15) PDOP in meters
16) HDOP in meters
17) VDOP in meters
18) Checksum
-----------------------------------------------------------------------------}
decodeGSA :: [Field] -> Either ParsingError NMEARecord
decodeGSA fields = Right $ GSA fields  -- TODO

{----------------------------------------------------------------------------
GSV: Satellites in view

       1 2 3 4 5 6 7     n
       | | | | | | |     |
$--GSV,x,x,x,x,x,x,x,...*hh
1) total number of messages
2) message number
3) satellites in view
4) satellite number
5) elevation in degrees
6) azimuth in degrees to true
7) SNR in dB
more satellite infos like 4)-7)
n) Checksum
-----------------------------------------------------------------------------}
decodeGSV :: [Field] -> Either ParsingError NMEARecord
decodeGSV fields = Right $ GSV fields  -- TODO

{----------------------------------------------------------------------------
RMC: Recommended Minimum Navigation Information

       1         2 3       4 5        6 7   8   9    10  11 12
       |         | |       | |        | |   |   |    |   |  |
$--RMC,hhmmss.ss,A,llll.ll,a,yyyyy.yy,a,x.x,x.x,xxxx,x.x,a*hh
 1) Time (UTC)
 2) Status, V = Navigation receiver warning
 3) Latitude
 4) N or S
 5) Longitude
 6) E or W
 7) Speed over ground, knots
 8) Track made good, degrees true
 9) Date, ddmmyy
10) Magnetic Variation, degrees
11) E or W
12) Checksum
-----------------------------------------------------------------------------}
decodeRMC :: [Field] -> Either ParsingError NMEARecord
decodeRMC fields = Right $ RMC fields  -- TODO

{----------------------------------------------------------------------------
VTG: Track Made Good and Ground Speed
       1   2 3   4 5 6 7   8 9
       |   | |   | | | |   | |
$--VTG,x.x,T,x.x,M,x.x,N,x.x,K*hh
1) Track Degrees
2) T = True
3) Track Degrees
4) M = Magnetic
5) Speed Knots
6) N = Knots
7) Speed Kilometers Per Hour
8) K = Kilometres Per Hour
9) Checksum
-----------------------------------------------------------------------------}
decodeVTG :: [Field] -> Either ParsingError NMEARecord
decodeVTG fields = Right $ VTG fields  -- TODO
