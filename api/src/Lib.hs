{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings, generateKey)
import Api

api :: Proxy NamedAPI
api = Proxy

startApp :: IO ()
startApp = do
    key <- generateKey
    let jwtCfg = defaultJWTSettings key
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    run 8080 $ serveWithContext api cfg $ Api.server defaultCookieSettings jwtCfg