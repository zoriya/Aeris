{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Api.About where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (defaultOptions, eitherDecode)
import qualified Data.Aeson.Parser
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Servant (Handler, RemoteHost)
import qualified Data.ByteString
import qualified Data.ByteString  as S
import qualified Data.ByteString.Lazy  as L

import Data.FileEmbed (embedDir)

data ClientAbout = ClientAbout
    { host :: String
    }
    deriving (Eq, Show)

data ActionAbout = ActionAbout
    { name :: String
    , description :: String
    }
    deriving (Eq, Show)

data ServicesAbout = ServicesAbout
    { name :: String
    , actions :: [ActionAbout]
    , reactions :: [ActionAbout]
    }
    deriving (Eq, Show, Generic)

data ServerAbout = ServerAbout
    { current_time :: POSIXTime
    , services :: [ServicesAbout]
    }
    deriving (Eq, Show)

data About = About
    { client :: ClientAbout
    , server :: ServerAbout
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''ClientAbout)
$(deriveJSON defaultOptions ''ActionAbout)
$(deriveJSON defaultOptions ''ServicesAbout)
$(deriveJSON defaultOptions ''ServerAbout)
$(deriveJSON defaultOptions ''About)

servicesDir :: [(FilePath, Data.ByteString.ByteString)]
servicesDir = $(embedDir "services")

about :: SockAddr -> AppM About
about host = do
    now <- liftIO getPOSIXTime
    let d = (eitherDecode . L.toStrict . snd <$> servicesDir) :: [Either String [ServicesAbout]]
    case d of
        Left err -> return $ About (ClientAbout $ show host) (ServerAbout now [])
        Right services -> return $ About (ClientAbout $ show host) (ServerAbout now services)
