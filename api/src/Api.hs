{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Servant.Auth.Server (CookieSettings, JWTSettings, JWT)
import Servant.API.Generic        ((:-), ToServantApi)
import Servant ( JSON, NamedRoutes, RemoteHost, type (:>), Get, HasServer (ServerT), Handler )
import GHC.Generics (Generic)

import Api.Auth
import Api.About

import Db.User ( User' )
import Api.User ( users )
import App
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

data API mode = API
    { users :: mode :- "users" :> Get '[JSON] [User']
    , about :: mode :- "about.json" :> RemoteHost :> Get '[JSON] About
    , auth :: mode :- "auth" :> NamedRoutes AuthAPI
    } deriving stock Generic

type NamedAPI = NamedRoutes API


server :: CookieSettings -> JWTSettings -> ServerT NamedAPI AppM
server cs jwts = API
  { Api.users = Api.User.users
  , Api.about = Api.About.about
  , Api.auth = Api.Auth.authHandler cs jwts
  }

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s