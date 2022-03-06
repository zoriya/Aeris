{-# LANGUAGE OverloadedStrings #-}

module Core.OIDC where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM

import App (AppM)
import Core.User (ExternalToken (ExternalToken, expiresAt), Service (Github, Reddit, Spotify, Google, Twitter, Anilist))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString, setRequestBodyURLEncoded)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObjString, lookupObjInt)
import Data.ByteString.Base64
import Data.Time (getCurrentTime, addUTCTime)
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
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            Just $ ExternalToken (pack access) "" currTime Github

-- Reddit
getRedditConfig :: IO OAuth2Conf
getRedditConfig =
    OAuth2Conf
        <$> envAsString "REDDIT_CLIENT_ID" ""
        <*> envAsString "REDDIT_SECRET" ""
        <*> pure "https://www.reddit.com/api/v1/access_token"

getRedditTokens :: String -> IO (Maybe ExternalToken)
getRedditTokens code = do
    cfg <- getRedditConfig
    let basicAuth = encodeBase64 $ B8.pack $ oauthClientId cfg ++ ":" ++ oauthClientSecret cfg
    let endpoint = tokenEndpoint code cfg
    request' <- parseRequest endpoint
    let request =
            setRequestMethod "POST" $
            addRequestHeader "Accept" "application/json" $
            addRequestHeader "User-Agent" "Aeris" $ 
            addRequestHeader "Authorization" (B8.pack $ "Basic " ++ unpack basicAuth) $
            setRequestBodyURLEncoded
                [ ("grant_type", "authorization_code")
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                ]
            request'
    response <- httpJSONEither request
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expiresIn <- lookupObjInt obj "expires_in"
            let expiresAt = addUTCTime (fromInteger . fromIntegral $ expiresIn) currTime
            Just $ ExternalToken (pack access) (pack refresh) expiresAt Reddit

-- GOOGLE
getGoogleConfig :: IO OAuth2Conf
getGoogleConfig =
    OAuth2Conf
        <$> envAsString "GOOGLE_CLIENT_ID" ""
        <*> envAsString "GOOGLE_SECRET" ""
        <*> pure "https://oauth2.googleapis.com/token"

getGoogleTokens :: String -> IO (Maybe ExternalToken)
getGoogleTokens code = do
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
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                ]
            request'
    response <- httpJSONEither request
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expiresIn <- lookupObjInt obj "expires_in"
            let expiresAt = addUTCTime (fromInteger . fromIntegral $ expiresIn) currTime
            Just $ ExternalToken (pack access) (pack refresh) expiresAt Google

-- SPOTIFY
getSpotifyConfig :: IO OAuth2Conf
getSpotifyConfig =
    OAuth2Conf
        <$> envAsString "SPOTIFY_CLIENT_ID" ""
        <*> envAsString "SPOTIFY_SECRET" ""
        <*> pure "https://accounts.spotify.com/api/token"

getSpotifyTokens :: String -> IO (Maybe ExternalToken)
getSpotifyTokens code = do
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
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                ]
            request'
    response <- httpJSONEither request
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expiresIn <- lookupObjInt obj "expires_in"
            let expiresAt = addUTCTime (fromInteger . fromIntegral $ expiresIn) currTime
            Just $ ExternalToken (pack access) (pack refresh) expiresAt Spotify

-- TWITTER
getTwitterConfig :: IO OAuth2Conf
getTwitterConfig =
    OAuth2Conf
        <$> envAsString "TWITTER_CLIENT_ID" ""
        <*> envAsString "TWITTER_SECRET" ""
        <*> pure "https://api.twitter.com/2/oauth2/token"

getTwitterTokens :: String -> IO (Maybe ExternalToken)
getTwitterTokens code = do
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
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                , ("code_verifier", "challenge")
                ]
            request'
    response <- httpJSONEither request
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expiresIn <- lookupObjInt obj "expires_in"
            let expiresAt = addUTCTime (fromInteger . fromIntegral $ expiresIn) currTime
            Just $ ExternalToken (pack access) (pack refresh) expiresAt Twitter

-- ANILIST
getAnilistConfig :: IO OAuth2Conf
getAnilistConfig =
    OAuth2Conf
        <$> envAsString "ANILIST_CLIENT_ID" ""
        <*> envAsString "ANILIST_SECRET" ""
        <*> pure "https://anilist.co/api/v2/oauth/token"

getAnilistTokens :: String -> IO (Maybe ExternalToken)
getAnilistTokens code = do
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
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                ]
            request'
    response <- httpJSONEither request
    currTime <- getCurrentTime
    return $ case (getResponseBody response :: Either JSONException Object) of
        Left _ -> Nothing
        Right obj -> do
            access <- lookupObjString obj "access_token"
            refresh <- lookupObjString obj "refresh_token"
            expiresIn <- lookupObjInt obj "expires_in"
            let expiresAt = addUTCTime (fromInteger . fromIntegral $ expiresIn) currTime
            Just $ ExternalToken (pack access) (pack refresh) expiresAt Anilist




-- General
getOauthTokens :: Service -> String -> IO (Maybe ExternalToken)
getOauthTokens Github = getGithubTokens
getOauthTokens Reddit = getRedditTokens
getOauthTokens Spotify = getSpotifyTokens
getOauthTokens Google = getGoogleTokens
getOauthTokens Twitter = getTwitterTokens
getOauthTokens Anilist = getAnilistTokens
