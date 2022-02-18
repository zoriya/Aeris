{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Core.Pipeline where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded), ReadShow (ReadShow))

data PipelineType = TwitterNewPost | TwitterNewFollower
    deriving stock (Generic, Read, Show)
    deriving (DBType) via ReadShow PipelineType
    deriving (FromJSON, ToJSON)

data TwitterNewPostData = TwitterNewPostData
    { author :: Text
    }
    deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterNewPostData)

data TwitterNewFollowerData = TwitterNewFollowerData
    { author :: Text
    }
    deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterNewFollowerData)

data PipelineParams
    = TwitterNewPostP TwitterNewPostData
    | TwitterNewFollowerP TwitterNewFollowerData
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving (DBType) via JSONBEncoded PipelineParams
