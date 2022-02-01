{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Lib
import qualified Hasql.Connection as Connection
import Db.User (userSchema, User (userId), insertUser)
import Rel8(each, select, insert)
import Control.Monad.IO.Class (liftIO)

import Hasql.Transaction ( Transaction, condemn, statement, sql )
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import Api.User (users)

main :: IO ()
main = do
    conn <- Connection.acquire connectionSettings
    case conn of
        Left _ -> putStrLn "Connection failed"
        Right c -> do
            e <- liftIO $ Session.run (Hasql.transaction Hasql.Serializable Hasql.Write (statement () (insert $ insertUser (head users )))) c
            case e of
                Left _ -> putStrLn "Query failed"
                Right d -> print $ head d
    where
        connectionSettings = Connection.settings "localhost" 5432 "postgres" "password" "postgres"
