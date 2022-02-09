{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Reaction where

import GHC.Generics (Generic)
import Rel8 (ReadShow (ReadShow), DBType, JSONBEncoded(JSONBEncoded))
import Data.Aeson ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)


data ReactionType = TwitterTweet | TwitterFollower
  deriving stock (Generic, Read, Show)
  deriving DBType via ReadShow ReactionType
  deriving (FromJSON, ToJSON)


data TwitterTweetData = TwitterTweetData
  { body :: Text
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterTweetData)

data TwitterFollowData = TwitterFollowData
  { toFollow :: Text
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterFollowData)


data ReactionParams =   TwitterTweetP       TwitterTweetData      |
                        TwitterFollowP      TwitterFollowData
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving DBType via JSONBEncoded ReactionParams