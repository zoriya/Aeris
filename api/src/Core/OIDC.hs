{-# LANGUAGE OverloadedStrings #-}

module Core.OIDC where
import Data.ByteString.Lazy

-- * OIDC

data OIDCConf =
  OIDCConf { redirectUri    :: ByteString
           , clientId       :: ByteString
           , clientPassword :: ByteString
           } deriving (Show, Eq)

oidcGoogleConf :: OIDCConf
oidcGoogleConf = OIDCConf
  { redirectUri    = "http://localhost:8080/auth/login/google"
  , clientId       = "914790981890-qjn5qjq5qjqjqjqjqjqjqjqjqjqjqjq.apps.googleusercontent.com"
  , clientPassword = "914790981890-qjn5qjq5qjqjqjqjqjqjqjqjqjqjqjqjq"
  }