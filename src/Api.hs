{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Either.Unwrap         (fromRight)
import Control.Monad.IO.Class     (liftIO)
import Data.List                  (sortBy)
import Data.Function              (on)
import Database.SQLite.Simple
import Servant
import Model

-- API endpoint definitions
-- /bridges?latitude<lat>&longitude<lng>
-- /stations?latitude<lat>&longitude<lng>
type API = "bridges"  :> QueryParam "latitude"  Double 
                      :> QueryParam "longitude" Double 
                      :> Get '[JSON] [Bridge]
      :<|> "stations" :> QueryParam "latitude"  Double
                      :> QueryParam "longitude" Double
                      :> Get '[JSON] [Station]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = queryBridges :<|> queryStations

-- Takes a latitude and longitude and returns all bridges ordered by distance
queryBridges :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Bridge]
queryBridges lat lng = 
    case lat of
        Nothing -> return []
        Just la -> case lng of 
                      Nothing -> return []
                      Just ln -> do
                          conn <- liftIO $ open "njdot-fuelup.db"
                          query <- liftIO $ query_ conn "SELECT * from bridges"
                          let s = getSorted $ bridgeDistances (la, ln) query
                          liftIO $ close conn
                          return s

-- Takes a latitude and longitude and returns all stations ordered by distance
queryStations :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Station]
queryStations lat lng =
    case lat of
        Nothing -> return []
        Just la -> case lng of 
                      Nothing -> return []
                      Just ln -> do
                          conn <- liftIO $ open "njdot-fuelup.db"
                          query <- liftIO $ query_ conn "SELECT * from stations"
                          let s = getSorted $ stationDistances (la, ln) query
                          liftIO $ close conn
                          return s

-- Haversine distance between two coordinates
haversine :: (Double, Double) -> (Double, Double) -> Double
haversine (lat1,lng1) (lat2,lng2) =  earthRadius * c
    where earthRadius = 6371
          radians n = n * pi / 180
          dLng = radians $ lng2 - lng1
          dLat = radians $ lat2 - lat1
          a = (sin (dLat/2))^2 + cos (radians lat1) 
              * cos (radians lat2) * (sin (dLng/2))^2
          c = 2 * (atan2 (sqrt a) (sqrt (1-a)))

-- Calculate distances to all bridges
bridgeDistances :: (Double, Double) -> [Bridge] -> [(Double, Bridge)]
bridgeDistances coord bridges = zipWith (,) (haversine coord <$> coords) bridges
    where coords = (\x -> (bridgeLatitude x, bridgeLongitude x)) <$> bridges

-- Calcuate distances to all stations
stationDistances :: (Double, Double) -> [Station] -> [(Double, Station)]
stationDistances c stations = zipWith (,) (haversine c <$> cs) stations
    where cs = (\x -> (stationLatitude x, stationLongitude x)) <$> stations

-- Sort locations based on haversine distance
getSorted :: [(Double,a)] -> [a]
getSorted list = snd <$> sorted
    where sorted = sortBy (compare `on` fst) list
