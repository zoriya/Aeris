{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Auth
import About
import User

type API = "users" :> Get '[JSON] [User]
      :<|> "about.json" :> RemoteHost :> Get '[JSON] About

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api Lib.server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> about

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]