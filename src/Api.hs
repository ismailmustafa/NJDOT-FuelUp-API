module Api (startApp) where

import Api.Internal

import Network.Wai.Handler.Warp

startApp :: IO ()
startApp = run 8080 app
