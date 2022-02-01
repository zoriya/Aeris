{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Db.User where

import Servant.Auth.JWT
import GHC.Generics (Generic)
import Data.Int
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Rel8

newtype UserId = UserId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num)
  deriving stock (Generic)

instance ToJSON UserId
instance FromJSON UserId

data User f = User
  { userId        :: Column f UserId
  , username      :: Column f Text
  , password      :: Column f Text
  , slug          :: Column f Text
  } deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (User f)

type User' = User Identity

instance ToJSON User'
instance ToJWT User'
instance FromJSON User'
instance FromJWT User'

userSchema :: TableSchema (User Name)
userSchema = TableSchema
  { name = "users"
  , schema = Nothing
  , columns = User
      { userId = "id"
      , username = "username"
      , password = "password"
      , slug = "slug"
      }
  }