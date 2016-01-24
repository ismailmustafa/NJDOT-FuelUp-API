{-# LANGUAGE TemplateHaskell #-}
module Model where

import Data.Aeson
import Data.Aeson.TH
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)

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

instance FromRow Station where
    fromRow = Station <$> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field

instance ToRow Station where
    toRow (Station a b c d e f g h i j k) = [toField a, toField b, toField c, toField d, toField e, toField f,
                                             toField g, toField h, toField i, toField j, toField k]

instance FromRow Bridge where
    fromRow = Bridge <$> field <*> field <*> field <*> field 
                     <*> field <*> field <*> field <*> field 
                     <*> field <*> field

instance ToRow Bridge where
    toRow (Bridge a b c d e f g h i j) = toRow (a,b,c,d,e,f,g,h,i,j)

