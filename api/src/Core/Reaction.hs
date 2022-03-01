{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Reaction where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode, Object)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded), ReadShow (ReadShow), DBEq)

data ReactionType = TwitterTweet | TwitterFollower
    deriving stock (Generic, Read, Show)
    deriving (DBType) via ReadShow ReactionType
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

newtype ReactionParams
    = ReactionParams { toObject :: Object }
    deriving stock (Generic)
    deriving newtype (DBEq, Eq, Show, FromJSON, ToJSON)
    deriving (DBType) via JSONBEncoded ReactionParams