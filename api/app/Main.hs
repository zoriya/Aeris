{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Hasql.Connection as Connection
import Rel8(each, select, insert)
import Control.Monad.IO.Class (liftIO)
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings, generateKey, JWTSettings, CookieSettings)
import Hasql.Pool (acquire)
import Hasql.Transaction ( Transaction, condemn, statement, sql )
import Servant

import Network.Wai
import Network.Wai.Handler.Warp
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )
import App
import Lib
import Config (getPostgresConfig, dbConfigToConnSettings)

main :: IO ()
main = do
    key <- generateKey
    dbConf <- getPostgresConfig
    appPort <- envAsInt "AERIS_BACK_PORT" 8080
    let jwtCfg = defaultJWTSettings key
    pool <- acquire (3, 1, dbConfigToConnSettings dbConf)
    run appPort $ app jwtCfg $ State pool
