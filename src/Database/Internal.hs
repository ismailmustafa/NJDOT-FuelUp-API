{-# LANGUAGE OverloadedStrings #-}

module Database.Internal where

import Text.CSV
import System.FilePath        ((</>))
import Data.Either.Unwrap     (fromRight)
import Model

type CSVData = [[String]]

-- STATIONS

-- Parse stations CSV data
parseStations :: IO CSVData
parseStations = do
    input <- readFile ("csv-data" </> "stations.csv")
    let inputClean = unlines . tail . lines $ input
    return . fromRight . parseCSV "" $ inputClean

-- Convert CSV stations to [Station]
csvToStations :: IO CSVData -> IO [Station]
csvToStations input = do
    csvList <- input
    return . extractValues $ csvToStation <$> csvList

-- CSV station row to Station
csvToStation :: [String] -> Maybe Station
csvToStation (a:b:c:d:e:f:g:h:i:j:k:[]) = 
    Just $ Station (read a :: Int) b (read c :: Double) 
                   (read d :: Double) e f g h i j k
csvToStation _ = Nothing

-- Throw away Nothing values
extractValues :: [Maybe a] -> [a]
extractValues [] = []
extractValues (x:xs) = case x of
                          Just m -> m : extractValues xs
                          _      -> extractValues xs

-- BRIDGES

-- Parse bridges CSV data
parseBridges :: IO CSVData
parseBridges = do
    input <- readFile ("csv-data" </> "bridges.csv")
    let inputClean = unlines . tail . lines $ input
    return . fromRight . parseCSV "" $ inputClean

-- Convert CSV bridges to [Bridge]
csvToBridges :: IO CSVData -> IO [Bridge]
csvToBridges input = do
    csvList <- input
    return . extractValues $ csvToBridge <$> csvList

-- CSV bridge row to Bridge
csvToBridge :: [String] -> Maybe Bridge
csvToBridge (a:b:c:d:e:f:g:h:i:j:[]) = 
    Just $ Bridge (read a :: Int) b c d e (read f :: Double) g h 
                  (read i :: Double) $ negate (read j :: Double)
csvToBridge _ = Nothing
