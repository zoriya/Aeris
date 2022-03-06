{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Api.About where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (defaultOptions, eitherDecode, Object)
import qualified Data.Aeson.Parser
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Servant (Handler, RemoteHost, throwError, err500, ServerError(..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Codec.Binary.UTF8.Generic as L

data ClientAbout = ClientAbout
    { host :: String
    }
    deriving (Eq, Show)

data ServerAbout = ServerAbout
    { current_time :: POSIXTime
    , services :: [Object]
    }
    deriving (Eq, Show)

data About = About
    { client :: ClientAbout
    , server :: ServerAbout
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''ClientAbout)
$(deriveJSON defaultOptions ''ServerAbout)
$(deriveJSON defaultOptions ''About)

servicesDir :: [(FilePath, S.ByteString)]
-- servicesDir = $(embedDir "./services/")
servicesDir = undefined

about :: SockAddr -> AppM About
about host = do
    now <- liftIO getPOSIXTime
    let d = traverse (eitherDecode . L.fromStrict . snd) servicesDir :: Either String [Object]
    case d of
        Left err -> throwError err500 { errBody = L.fromString err }
        Right services -> return $ About (ClientAbout $ show host) (ServerAbout now services)
