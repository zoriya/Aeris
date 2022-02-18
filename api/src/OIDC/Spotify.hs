{-# LANGUAGE OverloadedStrings #-}

module OIDC.Github where
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as B8

import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )
import Network.HTTP.Simple (JSONException, parseRequest, setRequestMethod, addRequestHeader, setRequestQueryString, httpJSONEither, getResponseBody)
import Data.Aeson.Types (Object, Value (String))
import Data.Text ( Text, pack )
import App (AppM)
import Core.User (ExternalToken (ExternalToken), Service (Github))


data GithubOAuth2 = GithubOAuth2
                    { oauthClientId :: String
                    , oauthClientSecret :: String
                    , oauthOAuthorizeEndpoint :: String
                    , oauthAccessTokenEndpoint :: String
                    , oauthCallback :: String
                    } deriving (Show, Eq)


getGithubConfig :: IO GithubOAuth2
getGithubConfig = GithubOAuth2
            <$> envAsString "GITHUB_CLIENT_ID" ""
            <*> envAsString "GITHUB_SECRET" ""
            <*> pure "https://github.com/login/oauth/authorize"
            <*> pure "https://github.com/login/oauth/access_token"
            <*> pure "http://localhost:8080/auth/github/token"


githubAuthEndpoint :: GithubOAuth2 -> String
githubAuthEndpoint oa = concat  [ oauthOAuthorizeEndpoint oa
                                , "?client_id=", oauthClientId oa
                                , "&response_type=", "code"
                                , "&redirect_uri=", oauthCallback oa]


tokenEndpoint :: String -> GithubOAuth2 -> String
tokenEndpoint code oa = concat  [ oauthAccessTokenEndpoint oa
                                , "?client_id=", oauthClientId oa
                                , "&client_secret=", oauthClientSecret oa
                                , "&code=", code]


lookupObj :: Object -> Text -> Maybe String
lookupObj obj key = case HM.lookup key obj of
                            Just (String x) -> Just . T.unpack $ x
                            _ -> Nothing

getGithubAuthEndpoint :: IO String
getGithubAuthEndpoint = githubAuthEndpoint <$> getGithubConfig

-- Step 3. Exchange code for auth token
getGithubTokens :: String -> IO (Maybe ExternalToken)
getGithubTokens code = do
  gh <- getGithubConfig
  let endpoint = tokenEndpoint code gh
  request' <- parseRequest endpoint
  let request = setRequestMethod "POST"
                $ addRequestHeader "Accept" "application/json"
                $ setRequestQueryString [ ("client_id", Just . B8.pack . oauthClientId $ gh)
                                        , ("client_secret", Just . B8.pack . oauthClientSecret $ gh)
                                        , ("code", Just . B8.pack $ code)]
                $ request'
  response <- httpJSONEither request
  return $ case (getResponseBody response :: Either JSONException Object) of
             Left _ -> Nothing
             Right obj -> do
               access <- lookupObj obj "access_token"
               refresh <- lookupObj obj "refresh_token"
               Just $ ExternalToken (pack access) (pack refresh) 0 Github