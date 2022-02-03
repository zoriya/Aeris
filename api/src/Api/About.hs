{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.About where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import qualified Data.Aeson.Parser
import Servant (Handler, RemoteHost)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Network.Socket (SockAddr)
import GHC.Generics ( Generic )
import App (AppM)

data ClientAbout = ClientAbout
  { host :: String
  } deriving (Eq, Show)

data ActionAbout = ActionAbout
  { name :: String
  , description :: String
  } deriving (Eq, Show)

data ServicesAbout = ServicesAbout
  { name :: String 
  , actions :: [ActionAbout]
  , reactions :: [ActionAbout]
  } deriving (Eq, Show, Generic)

data ServerAbout = ServerAbout
  { current_time :: POSIXTime 
  , services :: [ServicesAbout]
  } deriving (Eq, Show)

data About = About
  { client :: ClientAbout,
    server :: ServerAbout
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ClientAbout)
$(deriveJSON defaultOptions ''ActionAbout)
$(deriveJSON defaultOptions ''ServicesAbout)
$(deriveJSON defaultOptions ''ServerAbout)
$(deriveJSON defaultOptions ''About)

about :: SockAddr -> AppM About
about host = do
    now <- liftIO getPOSIXTime
    s <- liftIO (readFile "services.json")
    d <- liftIO ((eitherDecode <$> B.readFile "services.json") :: IO (Either String [ServicesAbout]))
    case d of
        Left err -> return $ About (ClientAbout $ show host) (ServerAbout now [])
        Right services -> return $ About (ClientAbout $ show host) (ServerAbout now services)