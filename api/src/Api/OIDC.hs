{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.OIDC where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Core.User (ExternalToken (ExternalToken, service), Service (Github), UserId (UserId), User (User))
import Data.Text (pack)
import Core.OIDC ( getOauthTokens )
import Repository.User (updateTokens, getTokensByUserId)
import Servant (Capture, Get, GetNoContent, JSON, NoContent (NoContent), QueryParam, ServerT, err400, throwError, type (:<|>) ((:<|>)), type (:>), err401, err403)
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)
import Utils (UserAuth, AuthRes)
import Servant.Auth.Server (AuthResult(Authenticated))

oauthHandler :: AuthRes -> Service -> Maybe String -> AppM NoContent
oauthHandler (Authenticated (User uid _ _)) service (Just code) = do
    tokens <- liftIO $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err403 
        Just t -> do
            updateTokens uid t
            return NoContent
oauthHandler _ service (Just code) = throwError err401
oauthHandler _ _ _ = throwError err400

servicesHandler :: AuthRes -> AppM [String]
servicesHandler (Authenticated (User uid name slug)) = do
    tokens <- getTokensByUserId uid
    return $ fmap (show . service) tokens
servicesHandler _ = throwError err401

type OauthAPI = UserAuth :> Capture "service" Service :> QueryParam "code" String :> Get '[JSON] NoContent
            :<|> UserAuth :> "services" :> Get '[JSON] [String]

oauth :: ServerT OauthAPI AppM
oauth = oauthHandler
    :<|> servicesHandler
