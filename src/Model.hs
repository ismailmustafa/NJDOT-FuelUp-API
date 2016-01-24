{-# LANGUAGE TemplateHaskell #-}

module Model where

import Data.Aeson
import Data.Aeson.TH
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)

-- Bridge model 
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

-- Station model
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

-- Magically derive JSON four our data types using template haskell
$(deriveJSON defaultOptions ''Bridge)
$(deriveJSON defaultOptions ''Station)

-- FromRow tells sqlite-simple how to convert a station entry
-- in our database to our Station data type
instance FromRow Station where
    fromRow = Station <$> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field <*> field 
                      <*> field <*> field <*> field

-- ToRow tells sqlite-simple how to convert our Station data type
-- to an sqlite entry
instance ToRow Station where
    toRow (Station a b c d e f g h i j k) = [toField a, toField b, toField c, 
                                             toField d, toField e, toField f,
                                             toField g, toField h, toField i, 
                                             toField j, toField k]

instance FromRow Bridge where
    fromRow = Bridge <$> field <*> field <*> field <*> field 
                     <*> field <*> field <*> field <*> field 
                     <*> field <*> field

instance ToRow Bridge where
    toRow (Bridge a b c d e f g h i j) = toRow (a,b,c,d,e,f,g,h,i,j)
