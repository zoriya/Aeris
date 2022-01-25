{-# LANGUAGE TemplateHaskell #-}

module About where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import qualified Data.Aeson.Parser
import Servant (Handler)
import Control.Monad.IO.Class (liftIO)

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

$(deriveJSON defaultOptions ''ClientAbout)
$(deriveJSON defaultOptions ''ActionAbout)
$(deriveJSON defaultOptions ''ServicesAbout)
$(deriveJSON defaultOptions ''ServerAbout)
$(deriveJSON defaultOptions ''About)

about :: Handler About
about = do
    now <- liftIO getPOSIXTime
    return $ About (ClientAbout "localhost") (ServerAbout now [])