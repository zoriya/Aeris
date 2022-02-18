{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.OIDC where

import OIDC
import Servant (NoContent (NoContent), type (:>), JSON, Get, ServerT, type (:<|>) ((:<|>)), QueryParam, throwError, err400, Capture, GetNoContent)
import Servant.Server.Generic (AsServerT)
import App (AppM)
import Servant.API.Generic (type (:-))
import Control.Monad.IO.Class (liftIO)
import Core.User (UserId(UserId), ExternalToken (ExternalToken, service), Service (Github))
import Repository.User (updateTokens)
import Data.Text (pack)

oauthHandler :: Service -> Maybe String -> AppM NoContent 
oauthHandler service (Just code) = do
    tokens <- liftIO $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err400
        Just t -> do
            updateTokens (UserId 1) t
            return NoContent
oauthHandler _ _ = throwError  err400

type OauthAPI =  Capture "service" Service :> QueryParam "code" String :> Get '[JSON] NoContent 
oauth :: ServerT OauthAPI AppM
oauth = oauthHandler