{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Hasql.Connection as Connection
import Hasql.Pool (acquire)
import Hasql.Transaction (Transaction, condemn, sql, statement)
import Rel8 (each, insert, select)
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)

import App
import Config (dbConfigToConnSettings, getPostgresConfig)
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import Lib
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)

main :: IO ()
main = do
    key <- generateKey
    dbConf <- getPostgresConfig
    appPort <- envAsInt "AERIS_BACK_PORT" 8080
    let jwtCfg = defaultJWTSettings key
    pool <- acquire (3, 1, dbConfigToConnSettings dbConf)
    run appPort $ app jwtCfg $ State pool
