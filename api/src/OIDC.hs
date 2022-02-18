module OIDC
( module OIDC.Github
, getOauthTokens
) where

import OIDC.Github
import Core.User (Service (Github), ExternalToken)

getOauthTokens :: Service -> String -> IO (Maybe ExternalToken)
getOauthTokens Github = getGithubTokens
getOauthTokens _ = \s -> return Nothing