{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Db.Pipeline where

import Data.Int (Int64)
import Data.Aeson
    ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Data.Aeson.TH ( deriveJSON )
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType, Column, Rel8able, ReadShow (ReadShow), JSONBEncoded (JSONBEncoded), Result, TableSchema (TableSchema, name, schema, columns), Name, Query, Expr, where_, (==.), lit, each, Insert (Insert, returning), into, rows, onConflict, returning, Returning (Projection), OnConflict (DoNothing), values, unsafeCastExpr, nextval)
import Data.Text (Text)

import Core.Pipeline
import Data.Functor.Identity (Identity)
newtype PipelineId = PipelineId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
  deriving stock (Generic)

data Pipeline f = Pipeline
  { pipelineId        :: Column f PipelineId
  , pipelineName      :: Column f Text
  , pipelineType      :: Column f PipelineType
  , pipelineParams    :: Column f PipelineParams
  } deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Pipeline f)

instance ToJSON (Pipeline Identity)
instance FromJSON (Pipeline Identity)

pipelineSchema :: TableSchema (Pipeline Name)
pipelineSchema = TableSchema
  { name = "pipelines"
  , schema = Nothing
  , columns = Pipeline
      { pipelineId = "id"
      , pipelineName = "name"
      , pipelineType = "type"
      , pipelineParams = "params"
      }
  }


selectAllPipelines :: Query (Pipeline Expr)
selectAllPipelines = each pipelineSchema

getPipelineById :: PipelineId -> Query (Pipeline Expr)
getPipelineById uid = do
    u <- selectAllPipelines
    where_ $ pipelineId u ==. lit uid
    return u


insertPipeline :: Pipeline Identity -> Insert [PipelineId]
insertPipeline (Pipeline _ name type' params) = Insert
    { into = pipelineSchema
    , rows = values [ Pipeline {
        pipelineId = unsafeCastExpr $ nextval "pipelines_id_seq",
        pipelineName = lit name,
        pipelineType = lit type',
        pipelineParams = lit params
    } ]
    , onConflict = DoNothing
    , returning = Projection pipelineId
    }