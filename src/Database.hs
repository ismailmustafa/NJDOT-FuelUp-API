{-# LANGUAGE OverloadedStrings #-}
module Database where

import Text.CSV
import System.FilePath ((</>))
import Data.Either.Unwrap (fromRight)
import Data.Maybe (fromJust)
import Database.SQLite.Simple
import Model

type CSVData = [[String]]

-- STATIONS

parseStations :: IO CSVData
parseStations = do
    input <- readFile ("csv-data" </> "stations.csv")
    let inputClean = unlines . tail . lines $ input
    return . fromRight . parseCSV "" $ inputClean

csvToStations :: IO CSVData -> IO [Station]
csvToStations input = do
    csvList <- input
    return . extractValues $ csvToStation <$> csvList

csvToStation :: [String] -> Maybe Station
csvToStation (a:b:c:d:e:f:g:h:i:j:k:[]) = 
    Just $ Station (read a :: Int) b (read c :: Double) (read d :: Double) e f g h i j k
csvToStation _ = Nothing

extractValues :: [Maybe a] -> [a]
extractValues [] = []
extractValues (x:xs) = case x of
                          Just m -> m : extractValues xs
                          _      -> extractValues xs

-- BRIDGES

parseBridges :: IO CSVData
parseBridges = do
    input <- readFile ("csv-data" </> "bridges.csv")
    let inputClean = unlines . tail . lines $ input
    return . fromRight . parseCSV "" $ inputClean

csvToBridges :: IO CSVData -> IO [Bridge]
csvToBridges input = do
    csvList <- input
    return . extractValues $ csvToBridge <$> csvList

csvToBridge :: [String] -> Maybe Bridge
csvToBridge (a:b:c:d:e:f:g:h:i:j:[]) = 
    Just $ Bridge (read a :: Int) b c d e (read f :: Double) g h (read i :: Double) $ negate (read j :: Double)
csvToBridge _ = Nothing

-- DATABASE

initializeDB :: IO ()
initializeDB = do
    conn <- open "njdot-fuelup.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS bridges (       \n\
                  \    bridgeId           INTEGER PRIMARY KEY,\n\
                  \    bridgeOwner        TEXT,               \n\
                  \    bridgeRoute        TEXT,               \n\
                  \    bridgeNumber       TEXT,               \n\
                  \    bridgeName         TEXT,               \n\
                  \    bridgeMp           REAL,               \n\
                  \    bridgeCounty       TEXT,               \n\
                  \    bridgeMunicipality TEXT,               \n\
                  \    bridgeLatitude     REAL,               \n\
                  \    bridgeLongitude    REAL                \n\
                  \)"
    execute_ conn "CREATE TABLE IF NOT EXISTS stations (      \n\
                  \    stationId          INTEGER PRIMARY KEY,\n\
                  \    stationName        TEXT,               \n\
                  \    stationLatitude    REAL,               \n\
                  \    stationLongitude   REAL,               \n\
                  \    stationAddress     TEXT,               \n\
                  \    stationCity        TEXT,               \n\
                  \    stationState       TEXT,               \n\
                  \    stationCounty      TEXT,               \n\
                  \    stationHours       TEXT,               \n\
                  \    stationPhoneNumber TEXT,               \n\
                  \    stationTypeGas     TEXT                \n\
                  \)"
    stations <- csvToStations parseStations
    bridges <- csvToBridges parseBridges
    sequence_ $ execute conn "INSERT INTO stations VALUES (?,?,?,?,?,?,?,?,?,?,?)" <$> stations
    sequence_ $ execute conn "INSERT INTO bridges VALUES (?,?,?,?,?,?,?,?,?,?)" <$> bridges
    close conn