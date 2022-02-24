{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Utils where

import Data.Aeson.Types (Value (String), Object)
import qualified Servant.Auth as Servant.Auth.Server
import Servant.Auth (JWT)
import Db.User (User')
import qualified Servant.Auth.Server
import Core.User (User)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Functor.Identity (Identity)
import Db.Pipeline (Pipeline (Pipeline), PipelineId (PipelineId), pipelineLastTrigger, pipelineTriggerCount, pipelineError, pipelineEnabled, pipelineUserId, pipelineParams, pipelineType, pipelineName, pipelineId)
import Core.Pipeline (PipelineType(TwitterNewPost), PipelineParams (TwitterNewPostP))


mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

lookupObj :: Object -> Text -> Maybe String
lookupObj obj key = case Data.HashMap.Strict.lookup key obj of
    Just (String x) -> Just . unpack $ x
    _ -> Nothing

defaultPipelineParams :: PipelineParams 
defaultPipelineParams = TwitterNewPostP (TwitterNewPostData "")

defaultPipeline :: Pipeline Identity
defaultPipeline = Pipeline
    { pipelineId = PipelineId 1
    , pipelineName = ""
    , pipelineType = TwitterNewPost
    , pipelineParams = defaultPipelineParams
    , pipelineUserId = UserId 1
    , pipelineEnabled = True
    , pipelineError = ""
    , pipelineTriggerCount = 0
    , pipelineLastTrigger = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
    }

type UserAuth = Servant.Auth.Server.Auth '[JWT] User
type AuthRes = Servant.Auth.Server.AuthResult User