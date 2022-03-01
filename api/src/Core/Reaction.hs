{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Core.Reaction where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded), ReadShow (ReadShow))

{--
data ReactionType = TwitterTweet | TwitterFollower
    deriving stock (Generic, Read, Show)
    deriving (DBType) via ReadShow ReactionType
    deriving (FromJSON, ToJSON)
--}

newtype ReactionType = ReactionType { toText :: Text }
    deriving stock (Generic, Read, Show)
    deriving (DBType) via ReadShow PipelineType
    deriving (FromJSON, ToJSON)

data TwitterTweetData = TwitterTweetData
    { body :: Text
    }
    deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterTweetData)

data TwitterFollowData = TwitterFollowData
    { toFollow :: Text
    }
    deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterFollowData)

data ReactionParams
    = TwitterTweetP TwitterTweetData
    | TwitterFollowP TwitterFollowData
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving (DBType) via JSONBEncoded ReactionParams
