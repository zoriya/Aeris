{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils where

import Data.Aeson.Types (Value (String), Object)
import qualified Servant.Auth as Servant.Auth.Server
import Servant.Auth (JWT)
import Db.User (User')
import qualified Servant.Auth.Server
import Core.User (User, UserId (UserId))
import Data.Text (Text, unpack)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Functor.Identity (Identity)
import Db.Pipeline (Pipeline (Pipeline), PipelineId (PipelineId), pipelineLastTrigger, pipelineTriggerCount, pipelineError, pipelineEnabled, pipelineUserId, pipelineParams, pipelineType, pipelineName, pipelineId)
import Core.Pipeline (PipelineType(TwitterNewPost), PipelineParams (TwitterNewPostP), TwitterNewPostData (TwitterNewPostData))
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Data.Default (Default)

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

lookupObj :: Object -> Text -> Maybe String
lookupObj obj key = case Data.HashMap.Strict.lookup key obj of
    Just (String x) -> Just . unpack $ x
    _ -> Nothing

defaultPipelineParams :: PipelineParams 
defaultPipelineParams = TwitterNewPostP (TwitterNewPostData "")

instance Default (Pipeline Identity) where
    def = Pipeline { pipelineId = PipelineId 1
    , pipelineName = ""
    , pipelineType = TwitterNewPost
    , pipelineParams = defaultPipelineParams
    , pipelineUserId = UserId 1
    , pipelineEnabled = True
    , pipelineError = Nothing
    , pipelineTriggerCount = 0
    , pipelineLastTrigger = Nothing
    }

type UserAuth = Servant.Auth.Server.Auth '[JWT] User
type AuthRes = Servant.Auth.Server.AuthResult User