{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.OIDC where


import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Core.User (ExternalToken (ExternalToken, service), Service (Github, Spotify, Twitter, Google, Anilist, Discord), UserId (UserId), User (User))
import Data.Text (pack, unpack)
import Core.OIDC ( getOauthTokens )
import Repository.User (updateTokens, getTokensByUserId, delTokens)
import Servant (Capture, Get, GetNoContent, JSON, NoContent (NoContent), QueryParam, ServerT, err400, throwError, type (:<|>) ((:<|>)), type (:>), err401, err403, ServerError (errHeaders), err302, Delete, Post)
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)
import Utils (UserAuth, AuthRes)
import qualified Data.ByteString.Char8 as B8
import Servant.Auth.Server (AuthResult(Authenticated), makeJWT)
import System.Environment.MrEnv (envAsString)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Db.User (toUser)

oauthHandler :: AuthRes -> Service -> Maybe String -> AppM NoContent
oauthHandler _ _ Nothing = throwError err400
oauthHandler (Authenticated (User uid _ _)) service (Just code) = do
    tokens <- liftIO $ runMaybeT $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err403 
        Just t -> do
            updateTokens uid t
            return NoContent
oauthHandler _ service (Just code) = throwError err401

oauthDelHandler :: AuthRes -> Service -> AppM NoContent 
oauthDelHandler (Authenticated (User uid _ _)) service = do
    delTokens uid service
    return NoContent 
oauthDelHandler _ _ = throwError err401

urlHandler :: Service -> Maybe String -> AppM NoContent
urlHandler _ Nothing = throwError err400
urlHandler Anilist (Just r) = do
    clientId <- liftIO $ envAsString "ANILIST_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://anilist.co/api/v2/oauth/authorize?client_id=" ++ clientId ++ "&response_type=code&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] } 
urlHandler Discord (Just r) = do
    clientId <- liftIO $ envAsString "DISCORD_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://discord.com/api/oauth2/authorize?response_type=code&scope=identify%20guilds%20messages.read%20activities.write%20webhook.incoming&client_id=" ++ clientId ++ "&response_type=code&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] } 
urlHandler Google  (Just r) = do
    clientId <- liftIO $ envAsString "GOOGLE_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://accounts.google.com/o/oauth2/v2/auth?scope=https://www.googleapis.com/auth/youtube.force-ssl&access_type=offline&prompt=consent&include_granted_scopes=true&response_type=code&client_id=" ++ clientId ++ "&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] } 
urlHandler Twitter (Just r) = do
    clientId <- liftIO $ envAsString "TWITTER_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://twitter.com/i/oauth2/authorize?response_type=code&scope=like.write like.read follows.read follows.write offline.access tweet.read tweet.write users.read&code_challenge=challenge&code_challenge_method=plain&client_id=" ++ clientId ++ "&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] } 
urlHandler Spotify (Just r) = do
    clientId <- liftIO $ envAsString "SPOTIFY_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://accounts.spotify.com/authorize?response_type=code&scope=user-library-read user-library-modify streaming playlist-modify-private playlist-read-collaborative playlist-read-private playlist-modify-public user-modify-playback-state user-read-private&client_id=" ++ clientId ++ "&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] }

urlHandler Github  (Just r) = do
    clientId <- liftIO $ envAsString "GITHUB_CLIENT_ID" ""
    backRedirect <- liftIO $ envAsString "BACK_URL" ""
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ "https://github.com/login/oauth/authorize?response_type=code&scope=repo&client_id=" ++ clientId ++ "&redirect_uri=" ++ backRedirect ++ "auth/redirect" ++ "&state=" ++ r)] } 

servicesHandler :: AuthRes -> AppM [String]
servicesHandler (Authenticated (User uid name slug)) = do
    tokens <- getTokensByUserId uid
    return $ fmap (show . service) tokens
servicesHandler _ = throwError err401

redirectHandler :: Maybe String -> Maybe String -> AppM NoContent
redirectHandler Nothing _ = return NoContent
redirectHandler _ Nothing = return NoContent
redirectHandler (Just code) (Just state) = do
    throwError $ err302 { errHeaders =
        [("Location", B8.pack $ state ++ "?code=" ++ code )] } 
    return NoContent

type OauthAPI = UserAuth :> Capture "service" Service :> QueryParam "code" String :> Get '[JSON] NoContent
            :<|> UserAuth :> Capture "service" Service :> Delete '[JSON] NoContent 
            :<|> Capture "service" Service :> "url" :> QueryParam "redirect_uri" String :> Get '[JSON] NoContent
            :<|> UserAuth :> "services" :> Get '[JSON] [String]
            :<|> "redirect" :> QueryParam "code" String :> QueryParam "state" String :> Get '[JSON] NoContent

oauth :: ServerT OauthAPI AppM
oauth = oauthHandler
    :<|> oauthDelHandler
    :<|> urlHandler
    :<|> servicesHandler
    :<|> redirectHandler