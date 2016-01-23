{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

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
    { stationId        :: Int
    , stationName      :: String
    , stationLatitude  :: Double
    , stationLongitude :: Double
    , stationAddress   :: String
    , stationCity      :: String
    , stationState     :: String
    , stationCounty    :: String
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Bridge)
$(deriveJSON defaultOptions ''Station)

type API = "bridges"  :> Get '[JSON] [Bridge]
      :<|> "stations" :> Get '[JSON] [Station]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return bridges :<|> return stations

bridges :: [Bridge]
bridges = [ Bridge 1 "State Routes" "1" "0902153" "SEACAUCUS RD OVER US 1&9 (TONNELLE AVE)" 0.48 "HUDSON" "North Bergen township" 40.7602 74.0511
          , Bridge 2 "State Routes" "1" "1101150" "ROUTE US 1 OVER ASSUNPINK CREEK" 0.88 "MERCER" "Trenton city" 40.2179 74.7557
          ]

stations :: [Station]
stations = [ Station 1 "Buena DOT" 39.5155 (-74.9285) "Rt. 40 near Catherine Avenue" "Buena" "NJ" "Atlantic"
           , Station 2 "Buena Vista State Police" 39.5779 (-74.8683) "1045 Rt. 54 South of Rt. 322" "Buena Vista Township" "NJ" "Atlantic"
           ]
