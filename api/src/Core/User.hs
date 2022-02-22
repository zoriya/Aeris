{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core.User where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType, JSONBEncoded (JSONBEncoded))
import Servant (AuthProtect, FromHttpApiData)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Auth.JWT (ToJWT, FromJWT)

newtype UserId = UserId {toInt64 :: Int64}
    deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
    deriving stock (Generic)

data Service = Github | Google | Spotify | Twitter | Discord
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData Service where
    parseUrlPiece :: Text -> Either Text Service
    parseUrlPiece "github" = Right Github
    parseUrlPiece "google" = Right Google
    parseUrlPiece "spotify" = Right Spotify
    parseUrlPiece "twitter" = Right Twitter
    parseUrlPiece "discord" = Right Discord
    parseUrlPiece _ = Left "not a service"

data ExternalToken = ExternalToken
    { accessToken :: Text
    , refreshToken :: Text
    , expiresIn :: Int64
    , service :: Service
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving (DBType) via JSONBEncoded ExternalToken

data User = User
    { userId :: UserId
    , userName :: Text
    , userSlug :: Text
    }
    deriving stock (Generic)

$(deriveJSON defaultOptions ''User)
instance FromJWT User
instance ToJWT User
