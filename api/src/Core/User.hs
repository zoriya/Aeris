{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.User where
import GHC.Generics (Generic)
import Data.Text (Text)
import Rel8 (DBType, DBEq, JSONBEncoded (JSONBEncoded))
import Data.Int (Int64)
import Data.Aeson.TH (deriveJSON)
import Servant.Server.Experimental.Auth (AuthServerData)
import Data.Aeson ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Servant (AuthProtect, FromHttpApiData)
import Servant.API (FromHttpApiData(parseUrlPiece))


newtype UserId = UserId { toInt64 :: Int64 }
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

data ExternalToken = ExternalToken {
  accessToken :: Text,
  refreshToken :: Text,
  expiresIn :: Int64,
  service :: Service
} deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving DBType via JSONBEncoded ExternalToken

data User = User 
  { userId        :: UserId
  , userName      :: Text
  , userSlug          :: Text
  } deriving stock (Generic)


$(deriveJSON defaultOptions ''User)