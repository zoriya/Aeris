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
import Api.User (users)
import App
import Lib

main :: IO ()
main = do
    key <- generateKey
    let jwtCfg = defaultJWTSettings key
    pool <- acquire (3, 1, connectionSettings)
    run 8080 $ app jwtCfg $ State pool
    where
        connectionSettings = Connection.settings "localhost" 5432 "postgres" "password" "postgres"
