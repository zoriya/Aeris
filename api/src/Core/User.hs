{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Core.User where
import GHC.Generics (Generic)
import Data.Text (Text)
import Rel8 (DBType, DBEq)
import Data.Int (Int64)
import Data.Aeson.TH (deriveJSON)
import Servant.Server.Experimental.Auth (AuthServerData)
import Data.Aeson ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Servant (AuthProtect)


newtype UserId = UserId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
  deriving stock (Generic)

data User = User 
  { userId        :: UserId
  , userName      :: Text
  , userSlug          :: Text
  } deriving stock (Generic)


$(deriveJSON defaultOptions ''User)