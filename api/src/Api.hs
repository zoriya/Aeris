{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import GHC.Generics (Generic)
import Servant (Get, Handler, HasServer (ServerT), JSON, NamedRoutes, RemoteHost, type (:>))
import Servant.API.Generic (ToServantApi, type (:-))
import Servant.Auth.Server (CookieSettings, JWT, JWTSettings)

import Api.About
import Api.Auth

import Db.User (User')

import Api.Pipeline
import qualified Api.Pipeline as Api
import App
import Control.Monad.Trans.Reader (ReaderT (runReaderT))

data API mode = API
    { about :: mode :- "about.json" :> RemoteHost :> Get '[JSON] About
    , auth :: mode :- "auth" :> NamedRoutes AuthAPI
    , pipelines :: mode :- "workflow" :> NamedRoutes PipelineAPI
    }
    deriving stock (Generic)

type NamedAPI = NamedRoutes API

server :: CookieSettings -> JWTSettings -> ServerT NamedAPI AppM
server cs jwts =
    API
        { Api.about = Api.About.about
        , Api.auth = Api.Auth.authHandler cs jwts
        , Api.pipelines = Api.Pipeline.pipelineHandler
        }

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s
