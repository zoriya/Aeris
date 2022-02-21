{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Pipeline where

import Data.Aeson (
    FromJSON,
    ToJSON,
    defaultOptions,
    eitherDecode,
 )
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (Column, DBEq, DBType, Expr, Insert (Insert, returning), JSONBEncoded (JSONBEncoded), Name, OnConflict (DoNothing), Query, ReadShow (ReadShow), Rel8able, Result, Returning (Projection), TableSchema (TableSchema, columns, name, schema), each, into, lit, nextval, onConflict, returning, rows, unsafeCastExpr, values, where_, (==.))

import Core.Pipeline
import Data.Functor.Identity (Identity)
import Servant (FromHttpApiData)
import Core.User (UserId(UserId))

newtype PipelineId = PipelineId {toInt64 :: Int64}
    deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON, FromHttpApiData)
    deriving stock (Generic)

data Pipeline f = Pipeline
    { pipelineId :: Column f PipelineId
    , pipelineName :: Column f Text
    , pipelineType :: Column f PipelineType
    , pipelineParams :: Column f PipelineParams
    , pipelineUserId :: Column f UserId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Pipeline f)

instance ToJSON (Pipeline Identity)
instance FromJSON (Pipeline Identity)

pipelineSchema :: TableSchema (Pipeline Name)
pipelineSchema =
    TableSchema
        { name = "pipelines"
        , schema = Nothing
        , columns =
            Pipeline
                { pipelineId = "id"
                , pipelineName = "name"
                , pipelineType = "type"
                , pipelineParams = "params"
                , pipelineUserId = "user_id" 
                }
        }

selectAllPipelines :: Query (Pipeline Expr)
selectAllPipelines = each pipelineSchema

getPipelineById :: PipelineId -> Query (Pipeline Expr)
getPipelineById uid = do
    u <- selectAllPipelines
    where_ $ pipelineId u ==. lit uid
    return u

getPipelineByUserId :: UserId -> Query (Pipeline Expr)
getPipelineByUserId uid = do
  u <- selectAllPipelines
  where_ $ pipelineUserId u ==. lit uid
  return u

insertPipeline :: Pipeline Identity -> Insert [PipelineId]
insertPipeline (Pipeline _ name type' params uid) =
    Insert
        { into = pipelineSchema
        , rows =
            values
                [ Pipeline
                    { pipelineId = unsafeCastExpr $ nextval "pipelines_id_seq"
                    , pipelineName = lit name
                    , pipelineType = lit type'
                    , pipelineParams = lit params
                    , pipelineUserId = lit uid
                    }
                ]
        , onConflict = DoNothing
        , returning = Projection pipelineId
        }
