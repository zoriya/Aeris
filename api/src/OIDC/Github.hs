{-# LANGUAGE OverloadedStrings #-}

module OIDC.Github where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM

import App (AppM)
import Core.User (ExternalToken (ExternalToken), Service (Github))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObj)

data OAuth2Conf = OAuth2Conf
    { oauthClientId :: String
    , oauthClientSecret :: String
    , oauthAccessTokenEndpoint :: String
    }
    deriving (Show, Eq)

getGithubConfig :: IO OAuth2Conf
getGithubConfig =
    OAuth2Conf
        <$> envAsString "GITHUB_CLIENT_ID" ""
        <*> envAsString "GITHUB_SECRET" ""
        <*> pure "https://github.com/login/oauth/access_token"

tokenEndpoint :: String -> OAuth2Conf -> String
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
    gh <- getGithubConfig
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
    print response
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObj obj "access_token"
            refresh <- lookupObj obj "refresh_token"
            Just $ ExternalToken (pack access) (pack refresh) 0 Github
