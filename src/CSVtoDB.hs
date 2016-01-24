{-# LANGUAGE OverloadedStrings #-}
module CSVtoDB where

import Text.CSV
import System.FilePath ((</>))
import Data.Either.Unwrap (fromRight)
import Data.Maybe (fromJust)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)
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
    return . extractStations $ csvToStation <$> csvList

csvToStation :: [String] -> Maybe Station
csvToStation (a:b:c:d:e:f:g:h:i:j:k:[]) = 
    Just $ Station (read a :: Int) b (read c :: Double) (read d :: Double) e f g h i j k
csvToStation _ = Nothing

extractStations :: [Maybe Station] -> [Station]
extractStations [] = []
extractStations (x:xs) = case x of
                          Just m -> m : extractStations xs
                          _      -> extractStations xs

instance FromRow Station where
    fromRow = Station <$> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field

instance ToRow Station where
    toRow (Station a b c d e f g h i j k) = [toField a, toField b, toField c, toField d, toField e, toField f,
                                             toField g, toField h, toField i, toField j, toField k]
persistStations :: IO ()
persistStations = do
    stations <- csvToStations parseStations
    conn <- open "njdot-fuelup.db"
    sequence_ $ execute conn "INSERT INTO stations VALUES (?,?,?,?,?,?,?,?,?,?,?)" <$> stations
    close conn

-- BRIDGES

parseBridges :: IO CSVData
parseBridges = do
    input <- readFile ("csv-data" </> "bridges.csv")
    let inputClean = unlines . tail . lines $ input
    return . fromRight . parseCSV "" $ inputClean

csvToBridges :: IO CSVData -> IO [Bridge]
csvToBridges input = do
    csvList <- input
    return . extractBridges $ csvToBridge <$> csvList

csvToBridge :: [String] -> Maybe Bridge
csvToBridge (a:b:c:d:e:f:g:h:i:j:[]) = 
    Just $ Bridge (read a :: Int) b c d e (read f :: Double) g h (read i :: Double) (read j :: Double)
csvToBridge _ = Nothing

extractBridges :: [Maybe Bridge] -> [Bridge]
extractBridges [] = []
extractBridges (x:xs) = case x of
                          Just m -> m : extractBridges xs
                          _      -> extractBridges xs

instance FromRow Bridge where
    fromRow = Bridge <$> field <*> field <*> field <*> field 
                     <*> field <*> field <*> field <*> field 
                     <*> field <*> field

instance ToRow Bridge where
    toRow (Bridge a b c d e f g h i j) = toRow (a,b,c,d,e,f,g,h,i,j)

persistBridges :: IO ()
persistBridges = do
    bridges <- csvToBridges parseBridges
    conn <- open "njdot-fuelup.db"
    sequence_ $ execute conn "INSERT INTO bridges VALUES (?,?,?,?,?,?,?,?,?,?)" <$> bridges
    close conn
