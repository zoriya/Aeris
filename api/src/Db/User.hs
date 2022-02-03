{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import Rel8 (Insert(onConflict), each)

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

selectAllUser :: Query (User Expr)
selectAllUser = each userSchema

getUserById :: Expr UserId -> Query (User Expr)
getUserById uid = do
    u <- selectAllUser
    where_ $ userId u ==. uid
    return u

getUserBySlug :: Expr Text -> Query (User Expr)
getUserBySlug s = do
    u <- selectAllUser
    where_ $ slug u ==. s
    return u

insertUser :: User' -> Insert [UserId]
insertUser usr = Insert
    { into = userSchema
    , rows = values [ User {
        userId = unsafeCastExpr $ nextval "users_id_seq",
        username = lit "bobby",
        password = lit "a",
        slug = lit "bobby"
    } ]
    , onConflict = DoNothing
    , returning = Projection userId
    }