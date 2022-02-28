{-# LANGUAGE OverloadedStrings #-}

module Core.OIDC where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM

import App (AppM)
import Core.User (ExternalToken (ExternalToken), Service (Github, Discord))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString, setRequestBodyURLEncoded)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObjString)

data OAuth2Conf = OAuth2Conf
    { oauthClientId :: String
    , oauthClientSecret :: String
    , oauthAccessTokenEndpoint :: String
    }
    deriving (Show, Eq)

tokenEndpoint :: String -> OAuth2Conf -> String
tokenEndpoint code oc =
    concat
        [ oauthAccessTokenEndpoint oc
        , "?client_id="
        , oauthClientId oc
        , "&client_secret="
        , oauthClientSecret oc
        , "&code="
        , code
        ]

-- GITHUB
getGithubConfig :: IO OAuth2Conf
getGithubConfig =
    OAuth2Conf
        <$> envAsString "GITHUB_CLIENT_ID" ""
        <*> envAsString "GITHUB_SECRET" ""
        <*> pure "https://github.com/login/oauth/access_token"

getGithubTokens :: String -> IO (Maybe ExternalToken)
getGithubTokens code = do
    gh <- getGithubConfig
    print gh
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
            request'
    response <- httpJSONEither request
    print response
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            Just $ ExternalToken (pack access) (pack refresh) 0 Github

-- DISCORD
getDiscordConfig :: IO OAuth2Conf
getDiscordConfig =
    OAuth2Conf
        <$> envAsString "DISCORD_CLIENT_ID" ""
        <*> envAsString "DISCORD_SECRET" ""
        <*> pure "https://discord.com/api/oauth2/token"

getDiscordTokens :: String -> IO (Maybe ExternalToken)
getDiscordTokens code = do
    cfg <- getDiscordConfig
    let endpoint = tokenEndpoint code cfg
    request' <- parseRequest endpoint
    let request =
            setRequestMethod "POST" $
            addRequestHeader "Accept" "application/json" $
            setRequestBodyURLEncoded
                [ ("client_id", B8.pack . oauthClientId $ cfg)
                , ("client_secret", B8.pack . oauthClientSecret $ cfg)
                , ("code", B8.pack code)
                , ("grant_type", "authorization_code")
                , ("redirect_uri", "http://localhost:3000/authorization/github")
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            Just $ ExternalToken (pack access) (pack refresh) 0 Github


-- General
getOauthTokens :: Service -> String -> IO (Maybe ExternalToken)
getOauthTokens Github = getGithubTokens
getOauthTokens Discord = getDiscordTokens
getOauthTokens _ = \s -> return Nothing
