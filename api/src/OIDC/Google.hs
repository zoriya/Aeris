{-# LANGUAGE OverloadedStrings #-}

module OIDC.Google where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import App (AppM)
import Core.User (ExternalToken (ExternalToken), Service (Github))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObj)

data GoogleOAuth2 = GoogleOAuth2
    { oauthClientId :: String
    , oauthClientSecret :: String
    , oauthAccessTokenEndpoint :: String
    }
    deriving (Show, Eq)

getGoogleConfig :: IO GoogleOAuth2
getGoogleConfig =
    GoogleOAuth2
        <$> envAsString "GOOGLE_CLIENT_ID" ""
        <*> envAsString "GOOGLE_SECRET" ""
        <*> pure "https://github.com/login/oauth/access_token"

tokenEndpoint :: String -> GoogleOAuth2 -> String
tokenEndpoint code oa =
    concat
        [ oauthAccessTokenEndpoint oa
        , "?client_id="
        , oauthClientId oa
        , "&client_secret="
        , oauthClientSecret oa
        , "&code="
        , code
        ]

-- Step 3. Exchange code for auth token
getGithubTokens :: String -> IO (Maybe ExternalToken)
getGithubTokens code = do
    gh <- getGoogleConfig
    let endpoint = tokenEndpoint code gh
    request' <- parseRequest endpoint
    let request =
            setRequestMethod "POST" $
                addRequestHeader "Accept" "application/json" $
                    setRequestQueryString
                        [ ("client_id", Just . B8.pack . oauthClientId $ gh)
                        , ("client_secret", Just . B8.pack . oauthClientSecret $ gh)
                        , ("code", Just . B8.pack $ code)
                        ]
                        $ request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObj obj "access_token"
            refresh <- lookupObj obj "refresh_token"
            Just $ ExternalToken (pack access) (pack refresh) 0 Github
