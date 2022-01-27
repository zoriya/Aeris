{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Hasql.Connection as Connection

main :: IO ()
main = do
    conn <- Connection.acquire connectionSettings
    case conn of
        Left _ -> putStrLn "Connection failed"
        Right c -> startApp
    where
        connectionSettings = Connection.settings "localhost" 5432 "postgres" "password" "postgres"
