module Api (startApp) where

import Internal.Api

import Network.Wai.Handler.Warp

startApp :: IO ()
startApp = run 8080 app
