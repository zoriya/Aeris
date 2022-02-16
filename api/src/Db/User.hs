{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Password (HashedPassword)

import Core.User
import Rel8 (Column, Rel8able, Result, TableSchema (TableSchema, schema, name, columns), Name, Query, Expr, Insert (Insert, returning), returning, onConflict, rows, into, where_, (==.), lit, values, unsafeCastExpr, nextval, OnConflict (DoNothing), Returning (Projection, NumberOfRowsAffected), each, Update (Update, target, updateWhere, from, set, returning))
import Data.List (findIndex)

data UserDB f = UserDB
  { userDBId        :: Column f UserId
  , username      :: Column f Text
  , password      :: Column f HashedPassword
  , slug          :: Column f Text
  , externalTokens :: Column f [ExternalToken]
  } deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (UserDB f)

type User' = UserDB Identity

instance ToJSON User'
instance ToJWT User'
instance FromJSON User'
instance FromJWT User'

toUser :: User' -> User
toUser (UserDB id name _ slug _) = User id name slug

userSchema :: TableSchema (UserDB Name)
userSchema = TableSchema
  { name = "users"
  , schema = Nothing
  , columns = UserDB
      { userDBId = "id"
      , username = "username"
      , password = "password"
      , slug = "slug"
      , externalTokens = "external_tokens"
      }
  }

selectAllUser :: Query (UserDB Expr)
selectAllUser = each userSchema

getUserById :: UserId -> Query (UserDB Expr)
getUserById uid = do
    u <- selectAllUser
    where_ $ userDBId u ==. lit uid
    return u

getUserByName :: Text -> Query (UserDB Expr)
getUserByName name = do
    u <- selectAllUser
    where_ $ username u ==. lit name
    return u

getUserBySlug :: Text -> Query (UserDB Expr)
getUserBySlug s = do
    u <- selectAllUser
    where_ $ slug u ==. lit s
    return u

insertUser :: User' -> Insert [UserId]
insertUser (UserDB id name password slug _) = Insert
    { into = userSchema
    , rows = values [ UserDB {
        userDBId = unsafeCastExpr $ nextval "users_id_seq",
        username = lit name,
        password = lit password,
        slug = lit slug,
        externalTokens = lit []
    } ]
    , onConflict = DoNothing
    , returning = Projection userDBId
    }

getUserTokensById :: UserId -> Query (Expr [ExternalToken])
getUserTokensById uid = externalTokens <$> getUserById uid

changeTokens :: [ExternalToken] -> ExternalToken -> [ExternalToken]
changeTokens actual new = do
    case findIndex (\t -> service t == service new) actual of
        Nothing -> new : actual
        Just idx -> let (x,_:ys) = splitAt idx actual
                    in x ++ new : ys

updateUserTokens :: UserId -> [ExternalToken ] -> ExternalToken -> Update Int64
updateUserTokens uid userTokens newToken = Update
    { target = userSchema
    , from = pure ()
    , updateWhere = \_ o -> userDBId o ==. lit uid
    , set = setter
    , returning = NumberOfRowsAffected
    }
    where
        setter = \from row -> row { externalTokens = lit $ changeTokens userTokens newToken}