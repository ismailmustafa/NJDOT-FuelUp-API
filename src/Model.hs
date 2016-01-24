{-# LANGUAGE TemplateHaskell #-}
module Model where

import Data.Aeson
import Data.Aeson.TH

data Bridge = Bridge
    { bridgeId           :: Int
    , bridgeOwner        :: String
    , bridgeRoute        :: String
    , bridgeNumber       :: String
    , bridgeName         :: String
    , bridgeMp           :: Double
    , bridgeCounty       :: String
    , bridgeMunicipality :: String
    , bridgeLatitude     :: Double
    , bridgeLongitude    :: Double
    } deriving (Eq, Show)

data Station = Station
    { stationId          :: Int
    , stationName        :: String
    , stationLatitude    :: Double
    , stationLongitude   :: Double
    , stationAddress     :: String
    , stationCity        :: String
    , stationState       :: String
    , stationCounty      :: String
    , stationHours       :: String
    , stationPhoneNumber :: String
    , stationTypeGas     :: String
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Bridge)
$(deriveJSON defaultOptions ''Station)
