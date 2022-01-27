{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Hasql.Connection as Connection

main :: IO ()
main = do
    Right conn <- Connection.acquire connectionSettings
    startApp
    where
        connectionSettings = Connection.settings "db" 5432 "postgres" "" "postgres"
