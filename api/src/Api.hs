{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Servant.Auth.Server (CookieSettings, JWTSettings, JWT)
import Servant.API.Generic        ((:-), ToServantApi)
import Servant ( JSON, NamedRoutes, RemoteHost, type (:>), Get, HasServer (ServerT), Handler )
import GHC.Generics (Generic)

import Api.Auth
import Api.About
import Api.User

data API mode = API
    { users :: mode :- "users" :> Get '[JSON] [User']
    , about :: mode :- "about.json" :> RemoteHost :> Get '[JSON] About
    , auth :: mode :- "auth" :> NamedRoutes AuthAPI
    } deriving stock Generic

type NamedAPI = NamedRoutes API


server :: CookieSettings -> JWTSettings -> ServerT NamedAPI Handler
server cs jwts = API {
    Api.users = return Api.User.users
    , Api.about = Api.About.about
    , Api.auth = Api.Auth.authHandler cs jwts
}