{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.OIDC where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Core.User (ExternalToken (ExternalToken, service), Service (Github), UserId (UserId), User (User))
import Data.Text (pack)
import OIDC
import Repository.User (updateTokens)
import Servant (Capture, Get, GetNoContent, JSON, NoContent (NoContent), QueryParam, ServerT, err400, throwError, type (:<|>) ((:<|>)), type (:>), err401)
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)
import Utils (UserAuth, AuthRes)
import Servant.Auth.Server (AuthResult(Authenticated))

oauthHandler :: AuthRes -> Service -> Maybe String -> AppM NoContent
oauthHandler (Authenticated (User uid name slug)) service (Just code) = do
    tokens <- liftIO $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err400
        Just t -> do
            updateTokens uid t
            return NoContent
oauthHandler _ service (Just code) = throwError err401
oauthHandler _ _ _ = throwError err400

type OauthAPI = UserAuth :> Capture "service" Service :> QueryParam "code" String :> Get '[JSON] NoContent

oauth :: ServerT OauthAPI AppM
oauth = oauthHandler
