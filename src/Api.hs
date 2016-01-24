{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Either.Unwrap (fromRight)
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
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

bridge :: [Bridge]
bridge = [Bridge 1 "" "" "" "" 2.0 "" "" 2.0 2.0]

station :: [Station]
station = [Station 1 "" 2.0 2.0 "" "" "" "" "" "" ""]

queryBridges :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Bridge]
queryBridges lat lng = do
    liftIO $ print lat
    liftIO $ print lng
    conn <- liftIO (open "njdot-fuelup.db")
    liftIO (query_ conn "SELECT * from bridges" :: IO [Bridge])

queryStations :: Maybe Double -> Maybe Double -> EitherT ServantErr IO [Station]
queryStations lat lng = do
    conn <- liftIO (open "njdot-fuelup.db")
    liftIO (query_ conn "SELECT * from stations" :: IO [Station])
