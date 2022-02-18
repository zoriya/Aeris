{-# LANGUAGE OverloadedStrings #-}

module OIDC.Discord where

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

data DiscordOAuth2 = DiscordOAuth2
    { oauthClientId :: String
    , oauthClientSecret :: String
    , oauthOAuthorizeEndpoint :: String
    , oauthAccessTokenEndpoint :: String
    , oauthCallback :: String
    }
    deriving (Show, Eq)

getDiscordConfig :: IO DiscordOAuth2
getDiscordConfig =
    DiscordOAuth2
        <$> envAsString "DISCORD_CLIENT_ID" ""
        <*> envAsString "DISCORD_SECRET" ""
        <*> pure "https://github.com/login/oauth/authorize"
        <*> pure "https://github.com/login/oauth/access_token"
        <*> pure "http://localhost:8080/auth/github/token"

githubAuthEndpoint :: DiscordOAuth2 -> String
githubAuthEndpoint oa =
    concat
        [ oauthOAuthorizeEndpoint oa
        , "?client_id="
        , oauthClientId oa
        , "&response_type="
        , "code"
        , "&redirect_uri="
        , oauthCallback oa
        ]

tokenEndpoint :: String -> DiscordOAuth2 -> String
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

getGithubAuthEndpoint :: IO String
getGithubAuthEndpoint = githubAuthEndpoint <$> getDiscordConfig

-- Step 3. Exchange code for auth token
getGithubTokens :: String -> IO (Maybe ExternalToken)
getGithubTokens code = do
    gh <- getDiscordConfig
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
