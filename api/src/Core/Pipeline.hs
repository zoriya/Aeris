{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Pipeline where

import GHC.Generics (Generic)
import Rel8 (DBType, ReadShow (ReadShow))
import Data.Aeson ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Data.Text (Text)
import Data.Aeson.TH (deriveJSON)

data PipelineType = TwitterNewPost | TwitterNewFollower
  deriving stock (Generic, Read, Show)
  deriving DBType via ReadShow PipelineType
  deriving (FromJSON, ToJSON)

data TwitterNewPostData = TwitterNewPostData
  { author :: Text
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterNewPostData)

data TwitterNewFollowerData = TwitterNewFollowerData
  { author :: Text
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TwitterNewFollowerData)