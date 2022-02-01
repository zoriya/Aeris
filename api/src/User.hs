

{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module User where

import GHC.Generics (Generic)
import Servant.Auth.JWT
-- import Servant.Auth.Server as SAS
import Data.Aeson (FromJSON, ToJSON)
import Rel8
import Prelude
import Data.Int
import Data.Functor.Identity (Identity)
newtype UserId = UserId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num)
  deriving stock (Generic)

data User f = User
  { userId        :: Column f UserId
  , username      :: Column f String
  , password      :: Column f String
  , slug          :: Column f String
  } deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (User f)

userSchema :: TableSchema (User Name)
userSchema = TableSchema
  { name = "user"
  , schema = Nothing
  , columns = User
      { userId = "id"
      , username = "name"
      , password = "password"
      , slug = "slug"
      }
  }

type User' = User Identity

users :: [User']
users = [
  User
    { userId = 1
    , username = "admin"
    , password = "passwd"
    , slug = "admin"
    }
  ]

instance ToJSON UserId
instance FromJSON UserId
instance ToJSON User'
instance ToJWT User'
instance FromJSON User'
instance FromJWT User'
