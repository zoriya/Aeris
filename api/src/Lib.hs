{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Servant.Auth.Server (CookieSettings, JWTSettings, JWT, defaultJWTSettings, defaultCookieSettings, generateKey)
import Network.Wai
import Servant.API.Generic        ((:-), ToServantApi)
import Network.Wai.Handler.Warp
import Servant
import Auth
import About
import User
import GHC.Generics (Generic)

-- type API = "users" :> Get '[JSON] [User]
--       :<|> "about.json" :> RemoteHost :> Get '[JSON] About

data API mode = API
    { users :: mode :- "users" :> Get '[JSON] [User]
    , about :: mode :- "about.json" :> RemoteHost :> Get '[JSON] About
    , auth :: mode :- "auth" :> NamedRoutes AuthAPI
    } deriving stock Generic

type NamedAPI = NamedRoutes API

startApp :: IO ()
startApp = do
    key <- generateKey
    let jwtCfg = defaultJWTSettings key
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    run 8080 $ serveWithContext api cfg $ Lib.server defaultCookieSettings jwtCfg

api :: Proxy NamedAPI
api = Proxy

server :: CookieSettings -> JWTSettings -> ServerT NamedAPI Handler
server cs jwts = API {
    Lib.users = return User.users
    , Lib.about = About.about
    , Lib.auth = Auth.authHandler cs jwts
}
