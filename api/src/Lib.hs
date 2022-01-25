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
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Parser

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data ClientAbout = ClientAbout
  { host :: String
  } deriving (Eq, Show)

data ActionAbout = ActionAbout
  { actionName :: String
  , description :: String
  } deriving (Eq, Show)

data ServicesAbout = ServicesAbout
  { name :: String 
  , actions :: [ActionAbout]
  , reactions :: [ActionAbout]
  } deriving (Eq, Show)

data ServerAbout = ServerAbout
  { current_time :: POSIXTime 
  , services :: [ServicesAbout]
  } deriving (Eq, Show)

data About = About
  { client :: ClientAbout,
    serverAbout :: ServerAbout
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''ClientAbout)
$(deriveJSON defaultOptions ''ActionAbout)
$(deriveJSON defaultOptions ''ServicesAbout)
$(deriveJSON defaultOptions ''ServerAbout)
$(deriveJSON defaultOptions ''About)


type API = "users" :> Get '[JSON] [User]
        :<|> "about.json" :> Get '[JSON] About

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> about
    where
        about :: Handler About
        about = do
            now <- liftIO getPOSIXTime
            return $ about' now

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

about' :: POSIXTime -> About
about' t = About (ClientAbout "localhost") (ServerAbout t [])
