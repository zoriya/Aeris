{-# LANGUAGE OverloadedStrings #-}

module Core.OIDC where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM

import App (AppM)
import Core.User (ExternalToken (ExternalToken), Service (Github, Discord, Spotify, Google, Twitter, Anilist))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString, setRequestBodyURLEncoded)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObjString, lookupObjInt)
import Data.ByteString.Base64
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

getGithubTokens :: String -> String -> IO (Maybe ExternalToken)
getGithubTokens code _ = do
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
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Github

-- DISCORD
getDiscordConfig :: IO OAuth2Conf
getDiscordConfig =
    OAuth2Conf
        <$> envAsString "DISCORD_CLIENT_ID" ""
        <*> envAsString "DISCORD_SECRET" ""
        <*> pure "https://discord.com/api/oauth2/token"

getDiscordTokens :: String -> String -> IO (Maybe ExternalToken)
getDiscordTokens code redirect = do
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
                , ("redirect_uri", B8.pack redirect)
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Discord

-- GOOGLE
getGoogleConfig :: IO OAuth2Conf
getGoogleConfig =
    OAuth2Conf
        <$> envAsString "GOOGLE_CLIENT_ID" ""
        <*> envAsString "GOOGLE_SECRET" ""
        <*> pure "https://oauth2.googleapis.com/token"

getGoogleTokens :: String -> String -> IO (Maybe ExternalToken)
getGoogleTokens code redirect = do
    cfg <- getGoogleConfig
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
                , ("redirect_uri", B8.pack redirect)
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Google

-- SPOTIFY
getSpotifyConfig :: IO OAuth2Conf
getSpotifyConfig =
    OAuth2Conf
        <$> envAsString "SPOTIFY_CLIENT_ID" ""
        <*> envAsString "SPOTIFY_SECRET" ""
        <*> pure "https://accounts.spotify.com/api/token"

getSpotifyTokens :: String -> String -> IO (Maybe ExternalToken)
getSpotifyTokens code redirect = do
    cfg <- getSpotifyConfig

    let basicAuth = encodeBase64 $ B8.pack $ oauthClientId cfg ++ ":" ++ oauthClientSecret cfg
    let endpoint = tokenEndpoint code cfg
    request' <- parseRequest endpoint
    let request =
            setRequestMethod "POST" $
            addRequestHeader "Authorization" (B8.pack $ "Basic " ++ unpack basicAuth) $
            addRequestHeader "Accept" "application/json" $
            setRequestBodyURLEncoded
                [ ("code", B8.pack code)
                , ("grant_type", "authorization_code")
                , ("redirect_uri", B8.pack redirect)
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Spotify

-- TWITTER
getTwitterConfig :: IO OAuth2Conf
getTwitterConfig =
    OAuth2Conf
        <$> envAsString "TWITTER_CLIENT_ID" ""
        <*> envAsString "TWITTER_SECRET" ""
        <*> pure "https://api.twitter.com/2/oauth2/token"

getTwitterTokens :: String -> String -> IO (Maybe ExternalToken)
getTwitterTokens code redirect = do
    cfg <- getTwitterConfig
    let basicAuth = encodeBase64 $ B8.pack $ "Basic " ++ oauthClientId cfg ++ ":" ++ oauthClientSecret cfg
    let endpoint = tokenEndpoint code cfg
    request' <- parseRequest endpoint
    let request =
            setRequestMethod "POST" $
            addRequestHeader "Authorization" (B8.pack . unpack $ basicAuth) $
            addRequestHeader "Accept" "application/json" $
            setRequestBodyURLEncoded
                [ ("code", B8.pack code)
                , ("grant_type", "authorization_code")
                , ("redirect_uri", B8.pack redirect)
                , ("code_verifier", "challenge")
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Twitter

-- ANILIST
getAnilistConfig :: IO OAuth2Conf
getAnilistConfig =
    OAuth2Conf
        <$> envAsString "ANILIST_CLIENT_ID" ""
        <*> envAsString "ANILIST_SECRET" ""
        <*> pure "https://anilist.co/api/v2/oauth/token"

getAnilistTokens :: String -> String -> IO (Maybe ExternalToken)
getAnilistTokens code redirect = do
    cfg <- getAnilistConfig
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
                , ("redirect_uri", B8.pack redirect)
                ]
            request'
    response <- httpJSONEither request
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expires_in <- lookupObjInt obj "expires_in"
            Just $ ExternalToken (pack access) (pack refresh) expires_in Anilist




-- General
getOauthTokens :: Service -> String -> String -> IO (Maybe ExternalToken)
getOauthTokens Github = getGithubTokens
getOauthTokens Discord = getDiscordTokens
getOauthTokens Spotify = getSpotifyTokens
getOauthTokens Google = getGoogleTokens
getOauthTokens Twitter = getTwitterTokens
getOauthTokens Anilist = getAnilistTokens
