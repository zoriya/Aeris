{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.OIDC where

import OIDC
import Servant (NoContent (NoContent), type (:>), JSON, Get, ServerT, type (:<|>) ((:<|>)), QueryParam, throwError, err400)
import Servant.Server.Generic (AsServerT)
import App (AppM)
import Servant.API.Generic (type (:-))
import Control.Monad.IO.Class (liftIO)
import Core.User (UserId(UserId), ExternalToken (ExternalToken))
import Repository.User (updateTokens)
import Data.Text (pack)

githubHandler :: AppM String
githubHandler = liftIO getGithubAuthEndpoint

githubCodeHandler :: Maybe String -> AppM NoContent 
githubCodeHandler (Just code) = do
    tokens <- liftIO $ getGithubTokens code
    case tokens of
        Nothing -> throwError err400
        Just (access, refresh) -> do
            let t = ExternalToken (pack access) (pack refresh) 0 "github"
            updateTokens (UserId 1) t
            return NoContent
githubCodeHandler Nothing = throwError  err400

type OauthAPI =  "github" :> Get '[JSON] String
            :<|> "github" :> "token" :> QueryParam "code" String :> Get '[JSON] NoContent 
oauth :: ServerT OauthAPI AppM
oauth = githubHandler
    :<|> githubCodeHandler