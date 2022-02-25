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
import Rel8 (Column, DBEq, DBType, Expr, Insert (Insert, returning), JSONBEncoded (JSONBEncoded), Name, OnConflict (DoNothing), Query, ReadShow (ReadShow), Rel8able, Result, Returning (Projection, NumberOfRowsAffected), TableSchema (TableSchema, columns, name, schema), each, into, lit, nextval, onConflict, returning, rows, unsafeCastExpr, values, where_, (==.), Update (Update, from, returning, set, target, updateWhere), target, from, Delete (Delete, deleteWhere, from, returning, using), target, from, using)

import Core.Pipeline ( PipelineParams, PipelineType )
import Data.Functor.Identity (Identity)
import Servant (FromHttpApiData)
import Core.User (UserId(UserId))
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)

newtype PipelineId = PipelineId {toInt64 :: Int64}
    deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON, FromHttpApiData)
    deriving stock (Generic)

data Pipeline f = Pipeline
    { pipelineId :: Column f PipelineId
    , pipelineName :: Column f Text
    , pipelineType :: Column f PipelineType
    , pipelineParams :: Column f PipelineParams
    , pipelineUserId :: Column f UserId
    , pipelineEnabled :: Column f Bool
    , pipelineError :: Column f Text
    , pipelineTriggerCount :: Column f Int64
    , pipelineLastTrigger :: Column f (Maybe UTCTime)
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
                , pipelineEnabled = "enabled"
                , pipelineError = "error"
                , pipelineTriggerCount = "trigger_count"
                , pipelineLastTrigger = "last_trigger"
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
insertPipeline (Pipeline _ name type' params uid _ _ _ _) =
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
                    , pipelineEnabled = lit True
                    , pipelineError = lit ""
                    , pipelineTriggerCount = lit 0
                    , pipelineLastTrigger = lit Nothing
                    }
                ]
        , onConflict = DoNothing
        , returning = Projection pipelineId
        }

deletePipeline :: PipelineId -> Delete Int64
deletePipeline pId =
    Delete
        { from = pipelineSchema
        , using = pure()
        , deleteWhere = \_ row -> pipelineId row ==. lit pId
        , returning = NumberOfRowsAffected
        }

updatePipeline :: PipelineId -> Pipeline Expr -> Update Int64
updatePipeline pId (Pipeline _ newName newType newParams _ newEnabled _ _ _) =
    Update
        { target = pipelineSchema
        , from = pure ()
        , updateWhere = \_ o -> pipelineId o ==. lit pId
        , set = setter
        , returning = NumberOfRowsAffected
        }
  where
    setter = \from row -> row {
      pipelineName = newName
    , pipelineType = newType
    , pipelineParams = newParams
    }