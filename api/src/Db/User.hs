{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Db.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Int
import Data.Text (Text)
import GHC.Generics (Generic)
import Password (HashedPassword)
import Servant.Auth.JWT

import Core.User
import Data.List (findIndex)
import Rel8 (Column, Expr, Insert (Insert, returning), Name, OnConflict (DoNothing), Query, Rel8able, Result, Returning (NumberOfRowsAffected, Projection), TableSchema (TableSchema, columns, name, schema), Update (Update, from, returning, set, target, updateWhere), each, into, lit, nextval, onConflict, returning, rows, unsafeCastExpr, values, where_, (==.))

data UserDB f = UserDB
    { userDBId :: Column f UserId
    , username :: Column f Text
    , password :: Column f HashedPassword
    , slug :: Column f Text
    , externalTokens :: Column f [ExternalToken]
    }
    deriving stock (Generic)
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
userSchema =
    TableSchema
        { name = "users"
        , schema = Nothing
        , columns =
            UserDB
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
insertUser (UserDB id name password slug _) =
    Insert
        { into = userSchema
        , rows =
            values
                [ UserDB
                    { userDBId = unsafeCastExpr $ nextval "users_id_seq"
                    , username = lit name
                    , password = lit password
                    , slug = lit slug
                    , externalTokens = lit []
                    }
                ]
        , onConflict = DoNothing
        , returning = Projection userDBId
        }

getUserTokensById :: UserId -> Query (Expr [ExternalToken])
getUserTokensById uid = externalTokens <$> getUserById uid

changeTokens :: [ExternalToken] -> ExternalToken -> [ExternalToken]
changeTokens actual new = do
    case findIndex (\t -> service t == service new) actual of
        Nothing -> new : actual
        Just idx ->
            let (x, _ : ys) = splitAt idx actual
             in x ++ new : ys

updateUserTokens :: UserId -> [ExternalToken] -> ExternalToken -> Update Int64
updateUserTokens uid userTokens newToken =
    Update
        { target = userSchema
        , from = pure ()
        , updateWhere = \_ o -> userDBId o ==. lit uid
        , set = setter
        , returning = NumberOfRowsAffected
        }
  where
    setter = \from row -> row{externalTokens = lit $ changeTokens userTokens newToken}
