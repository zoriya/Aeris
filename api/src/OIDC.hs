module OIDC (
    module OIDC.Github,
    getOauthTokens,
) where

import Core.User (ExternalToken, Service (Github))
import OIDC.Github

getOauthTokens :: Service -> String -> IO (Maybe ExternalToken)
getOauthTokens Github = getGithubTokens
getOauthTokens _ = \s -> return Nothing
