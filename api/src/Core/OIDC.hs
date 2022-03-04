{-# LANGUAGE OverloadedStrings #-}

module Core.OIDC where

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM

import App (AppM)
import Core.User (ExternalToken (ExternalToken, accessToken, providerId), Service (Github, Discord, Spotify, Google, Twitter, Anilist))
import Data.Aeson.Types (Object, Value (String))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Simple (JSONException, addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestMethod, setRequestQueryString, setRequestBodyURLEncoded)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import Utils (lookupObjString, lookupObjObject, lookupObjInt)
import Data.ByteString.Base64
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (MonadPlus (mzero))
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

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return


-- GITHUB
getGithubConfig :: IO OAuth2Conf
getGithubConfig =
    OAuth2Conf
        <$> envAsString "GITHUB_CLIENT_ID" ""
        <*> envAsString "GITHUB_SECRET" ""
        <*> pure "https://github.com/login/oauth/access_token"

getGithubTokens :: String -> MaybeT IO ExternalToken
getGithubTokens code = MaybeT $ do
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
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    let t = ExternalToken access "" 0 Github Nothing
    githubId <- runMaybeT $ getGithubId t
    return $ Just $ t { providerId = githubId }

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

getGithubId :: ExternalToken -> MaybeT IO Text
getGithubId t = MaybeT $ do
    let endpoint = "https://api.github.com/user"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Authorization" (B8.pack ("token " ++ unpack (accessToken t))) $
            addRequestHeader "Accept" "application/json" $
            addRequestHeader "User-Agent" "aeris-server"
            request'
    response <- httpJSONEither request
    print $ accessToken t
    case (getResponseBody response :: Either JSONException Object) of
        Left err -> return Nothing
        Right obj -> case lookupObjInt obj "id" of
            Just githubId -> return $ Just $ pack $ show githubId
            _ -> return Nothing

-- DISCORD
getDiscordConfig :: IO OAuth2Conf
getDiscordConfig =
    OAuth2Conf
        <$> envAsString "DISCORD_CLIENT_ID" ""
        <*> envAsString "DISCORD_SECRET" ""
        <*> pure "https://discord.com/api/oauth2/token"

getDiscordTokens :: String -> MaybeT IO ExternalToken
getDiscordTokens code = MaybeT $ do
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
                , ("redirect_uri", "http://localhost:8080/auth/redirect")
                ]
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    refresh <- liftMaybe $ lookupObjString obj "refresh_token"
    let t = ExternalToken access refresh 0 Discord Nothing
    discordId <- runMaybeT $ getDiscordId t
    return $ Just $ t { providerId = discordId }


getDiscordId :: ExternalToken -> MaybeT IO Text
getDiscordId t = MaybeT $ do
    let endpoint = "https://discord.com/api/users/@me"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Accept" "application/json" $
            addRequestHeader "Authorization" (B8.pack $ "Bearer " ++ unpack (accessToken t))
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    return $ lookupObjString obj "id"


-- GOOGLE
getGoogleConfig :: IO OAuth2Conf
getGoogleConfig =
    OAuth2Conf
        <$> envAsString "GOOGLE_CLIENT_ID" ""
        <*> envAsString "GOOGLE_SECRET" ""
        <*> pure "https://oauth2.googleapis.com/token"

getGoogleTokens :: String -> MaybeT IO ExternalToken
getGoogleTokens code = MaybeT $ do
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
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    refresh <- liftMaybe $ lookupObjString obj "refresh_token"
    let t = ExternalToken access refresh 0 Google Nothing
    googleId <- runMaybeT $ getGoogleId t
    return $ Just $ t { providerId = googleId }

getGoogleId :: ExternalToken -> MaybeT IO Text
getGoogleId t = MaybeT $ do
    let endpoint = "https://oauth2.googleapis.com/tokeninfo"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Accept" "application/json" $
            setRequestQueryString
                [ ("access_token", Just . B8.pack . unpack $ accessToken t)
                ]
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    return $ lookupObjString obj "sub"

-- SPOTIFY
getSpotifyConfig :: IO OAuth2Conf
getSpotifyConfig =
    OAuth2Conf
        <$> envAsString "SPOTIFY_CLIENT_ID" ""
        <*> envAsString "SPOTIFY_SECRET" ""
        <*> pure "https://accounts.spotify.com/api/token"

getSpotifyTokens :: String -> MaybeT IO ExternalToken
getSpotifyTokens code = MaybeT $ do
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
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    refresh <- liftMaybe $ lookupObjString obj "refresh_token"
    let t = ExternalToken access refresh 0 Spotify Nothing
    spotifyId <- runMaybeT $ getSpotifyId t
    return $ Just $ t { providerId = spotifyId }

getSpotifyId :: ExternalToken -> MaybeT IO Text
getSpotifyId t = MaybeT $ do
    let endpoint = "https://api.spotify.com/v1/me"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Content-Type" "application/json" $
            addRequestHeader "Authorization" (B8.pack $ "Bearer " ++ unpack (accessToken t))
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    return $ lookupObjString obj "id"

-- TWITTER
getTwitterConfig :: IO OAuth2Conf
getTwitterConfig =
    OAuth2Conf
        <$> envAsString "TWITTER_CLIENT_ID" ""
        <*> envAsString "TWITTER_SECRET" ""
        <*> pure "https://api.twitter.com/2/oauth2/token"

getTwitterTokens :: String -> MaybeT IO ExternalToken
getTwitterTokens code = MaybeT $ do
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
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    refresh <- liftMaybe $ lookupObjString obj "refresh_token"
    let t = ExternalToken access refresh 0 Twitter Nothing
    twitterId <- runMaybeT $ getTwitterId t
    return $ Just $ t { providerId = twitterId }

getTwitterId :: ExternalToken -> MaybeT IO Text
getTwitterId t = MaybeT $ do
    let endpoint = "https://api.twitter.com/2/users/me"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Content-Type" "application/json" $
            addRequestHeader "Authorization" (B8.pack $ "Bearer " ++ unpack (accessToken t))
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    case lookupObjObject obj "data" of
        Just dataBody -> return $ lookupObjString dataBody "id"
        _ -> return Nothing

-- ANILIST
getAnilistConfig :: IO OAuth2Conf
getAnilistConfig =
    OAuth2Conf
        <$> envAsString "ANILIST_CLIENT_ID" ""
        <*> envAsString "ANILIST_SECRET" ""
        <*> pure "https://anilist.co/api/v2/oauth/token"

getAnilistTokens :: String -> MaybeT IO ExternalToken
getAnilistTokens code = MaybeT $ do
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
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    access <- liftMaybe $ lookupObjString obj "access_token"
    refresh <- liftMaybe $ lookupObjString obj "refresh_token"
    let t = ExternalToken access refresh 0 Anilist Nothing
    anilistId <- runMaybeT $ getAnilistId t
    return $ Just $ t { providerId = anilistId }


getAnilistId :: ExternalToken -> MaybeT IO Text
getAnilistId t = MaybeT $ do
    let endpoint = "https://api.twitter.com/2/users/me"
    request' <- parseRequest endpoint
    let request =
            addRequestHeader "Content-Type" "application/json" $
            addRequestHeader "Authorization" (B8.pack $ "Bearer " ++ unpack (accessToken t))
            request'
    response <- httpJSONEither request
    let (Right obj) = (getResponseBody response :: Either JSONException Object)
    dataBody <- liftMaybe $ lookupObjObject obj "data"
    viewer <- liftMaybe $ lookupObjObject obj "Viewer"
    return . Just . pack . show $ lookupObjInt viewer "id"

-- General
getOauthTokens :: Service -> String -> MaybeT IO ExternalToken
getOauthTokens Github = getGithubTokens
getOauthTokens Discord = getDiscordTokens
getOauthTokens Spotify = getSpotifyTokens
getOauthTokens Google = getGoogleTokens
getOauthTokens Twitter = getTwitterTokens
getOauthTokens Anilist = getAnilistTokens
