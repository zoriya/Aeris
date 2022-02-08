{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}


module Db.Pipeline where

import Data.Int (Int64)
import Data.Aeson
    ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Data.Aeson.TH ( deriveJSON )
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType, Column, Rel8able, ReadShow (ReadShow), JSONBEncoded (JSONBEncoded))
import Data.Text (Text)


newtype PipelineId = PipelineId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
  deriving stock (Generic)

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

data PipelineParams =   TwitterNewPostP     TwitterNewPostData      |
                        TwitterNewFollowerP TwitterNewFollowerData
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving DBType via JSONBEncoded PipelineParams
data Pipeline f = Pipeline
  { pipelineId        :: Column f PipelineId
  , pipelineName      :: Column f Text
  , pipelineType      :: Column f PipelineType
  , pipelineParams    :: Column f PipelineParams
  } deriving stock (Generic)
    deriving anyclass (Rel8able)