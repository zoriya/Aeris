{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Db.Pipeline where

import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType, Column, Rel8able, ReadShow (ReadShow))
import Data.Text (Text)


newtype PipelineId = PipelineId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
  deriving stock (Generic)

data PipelineType = TwitterNewPost | TwitterNewFollower
  deriving stock (Generic, Read, Show)
  deriving DBType via ReadShow PipelineType
  deriving (FromJSON, ToJSON)

data Pipeline f = Pipeline
  { pipelineId        :: Column f PipelineId
  , pipelineName      :: Column f Text
  , pipelineType      :: Column f PipelineType
  } deriving stock (Generic)
    deriving anyclass (Rel8able)