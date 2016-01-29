{-# LANGUAGE OverloadedStrings #-}

module Database (initializeDB) where

import Internal.Database
import Database.SQLite.Simple

-- Create tables and insert CSV data into database
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
    sequence_ $ execute conn "INSERT INTO stations VALUES \
                             \(?,?,?,?,?,?,?,?,?,?,?)" <$> stations
    sequence_ $ execute conn "INSERT INTO bridges VALUES \
                             \(?,?,?,?,?,?,?,?,?,?)" <$> bridges
    close conn
