{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Utils where

import Data.Aeson.Types (Value (String), Object)
import qualified Servant.Auth as Servant.Auth.Server
import Servant.Auth (JWT)
import Db.User (User')
import qualified Servant.Auth.Server
import Core.User (User)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (HashMap, lookup)


mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

lookupObj :: Object -> Text -> Maybe String
lookupObj obj key = case Data.HashMap.Strict.lookup key obj of
    Just (String x) -> Just . unpack $ x
    _ -> Nothing

type UserAuth = Servant.Auth.Server.Auth '[JWT] User
type AuthRes = Servant.Auth.Server.AuthResult User