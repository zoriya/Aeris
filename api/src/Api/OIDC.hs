{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.OIDC where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Core.User (ExternalToken (ExternalToken, service), Service (Github), UserId (UserId))
import Data.Text (pack)
import OIDC
import Repository.User (updateTokens)
import Servant (Capture, Get, GetNoContent, JSON, NoContent (NoContent), QueryParam, ServerT, err400, throwError, type (:<|>) ((:<|>)), type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)

oauthHandler :: Service -> Maybe String -> AppM NoContent
oauthHandler service (Just code) = do
    tokens <- liftIO $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err400
        Just t -> do
            updateTokens (UserId 1) t
            return NoContent
oauthHandler _ _ = throwError err400

type OauthAPI = Capture "service" Service :> QueryParam "code" String :> Get '[JSON] NoContent
oauth :: ServerT OauthAPI AppM
oauth = oauthHandler
