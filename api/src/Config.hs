{-# LANGUAGE OverloadedStrings #-}

module Config where

import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )
import qualified Hasql.Connection as Connection
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.Word (Word16)

data DbConfig = DbConfig
    { dbPort        :: Int
    , dbHost        :: String
    , dbDB          :: String
    , dbUser        :: String
    , dbPassword    :: String
    }

getPostgresConfig :: IO DbConfig
getPostgresConfig = DbConfig
    <$> envAsInt "POSTGRES_PORT" 5432
    <*> envAsString "POSTGRES_HOST" "localhost"
    <*> envAsString "POSTGRES_DB" "postgres"
    <*> envAsString "POSTGRES_USER" "postgres"
    <*> envAsString "POSTGRES_PASSWORD" "password"

dbConfigToConnSettings :: DbConfig -> Connection.Settings
dbConfigToConnSettings (DbConfig port host db user passwd) = Connection.settings
    (encodeUtf8 $ pack host)
    (fromIntegral port :: Word16)
    (encodeUtf8 $ pack user)
    (encodeUtf8 $ pack passwd)
    (encodeUtf8 $ pack db)
