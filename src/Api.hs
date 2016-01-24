{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( startApp,
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Either.Unwrap (fromRight)
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Function (on)
import Servant
import Model

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

queryBridges :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Bridge]
queryBridges lat lng = 
    case lat of
        Nothing -> return []
        Just la -> case lng of 
                      Nothing -> return []
                      Just ln -> do
                          conn <- liftIO $ open "njdot-fuelup.db"
                          query <- liftIO $ query_ conn "SELECT * from bridges"
                          let sorted = getSorted $ bridgeDistances (la, ln) query
                          liftIO $ close conn
                          return sorted

queryStations :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Station]
queryStations lat lng =
    case lat of
        Nothing -> return []
        Just la -> case lng of 
                      Nothing -> return []
                      Just ln -> do
                          conn <- liftIO $ open "njdot-fuelup.db"
                          query <- liftIO $ query_ conn "SELECT * from stations"
                          let sorted = getSorted $ stationDistances (la, ln) query
                          liftIO $ close conn
                          return query

-- Calculate Haversine

haversine :: (Double, Double) -> (Double, Double) -> Double
haversine (lat1,lng1) (lat2,lng2) =  earthRadius * c
    where earthRadius = 6371
          radians n = n * pi / 180
          dLng = radians $ lng2 - lng1
          dLat = radians $ lat2 - lat1
          a = (sin (dLat/2))^2 + cos (radians lat1) * cos (radians lat2) * (sin (dLng/2))^2
          c = 2 * (atan2 (sqrt a) (sqrt (1-a)))

bridgeDistances :: (Double, Double) -> [Bridge] -> [(Double, Bridge)]
bridgeDistances coord bridges = zipWith (,) (haversine coord <$> coords) bridges
    where coords = (\x -> (bridgeLatitude x, bridgeLongitude x)) <$> bridges

stationDistances :: (Double, Double) -> [Station] -> [(Double, Station)]
stationDistances coord stations = zipWith (,) (haversine coord <$> coords) stations
    where coords = (\x -> (stationLatitude x, stationLongitude x)) <$> stations

getSorted :: [(Double,a)] -> [a]
getSorted list = snd <$> sorted
    where sorted = sortBy (compare `on` fst) list










