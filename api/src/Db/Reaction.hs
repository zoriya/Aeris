{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Reaction where

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
import Rel8 (Column, DBEq, DBType, Expr, Insert (Insert, returning), JSONBEncoded (JSONBEncoded), Name, OnConflict (DoNothing), Query, ReadShow (ReadShow), Rel8able, Result, Returning (Projection), TableSchema (TableSchema, columns, name, schema), each, into, lit, nextval, onConflict, returning, rows, unsafeCastExpr, values, where_, (==.), many, ListTable)

import Core.Reaction
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId (PipelineId), pipelineSchema, Pipeline (pipelineId), getPipelineById, getPipelineByUserId)
import Core.User (UserId)

newtype ReactionId = ReactionId {toInt64 :: Int64}
    deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
    deriving stock (Generic)

data Reaction f = Reaction
    { reactionId :: Column f ReactionId
    , reactionType :: Column f ReactionType
    , reactionParams :: Column f ReactionParams
    , reactionPipelineId :: Column f PipelineId
    , reactionOrder :: Column f Int64
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Reaction f)

instance ToJSON (Reaction Identity)
instance FromJSON (Reaction Identity)

reactionSchema :: TableSchema (Reaction Name)
reactionSchema =
    TableSchema
        { name = "reactions"
        , schema = Nothing
        , columns =
            Reaction
                { reactionId = "id"
                , reactionOrder = "react_order"
                , reactionType = "type"
                , reactionParams = "params"
                , reactionPipelineId = "pipeline_id"
                }
        }

insertReaction :: Reaction Identity -> Insert [ReactionId]
insertReaction (Reaction _ type' params pipeId order) =
    Insert
        { into = reactionSchema
        , rows =
            values
                [ Reaction
                    { reactionId = unsafeCastExpr $ nextval "reactions_id_seq"
                    , reactionType = lit type'
                    , reactionParams = lit params
                    , reactionPipelineId = lit pipeId
                    , reactionOrder = lit order
                    }
                ]
        , onConflict = DoNothing
        , returning = Projection reactionId
        }

getReactionsByPipelineId :: PipelineId -> Query (Reaction Expr)
getReactionsByPipelineId pId = do
    r <- each reactionSchema
    where_ $ reactionPipelineId r ==. lit pId
    return r

reactionsForPipeline :: (Column f PipelineId ~ Expr PipelineId) => Pipeline f -> Query (Reaction Expr)
reactionsForPipeline pipeline = do
  reaction <- each reactionSchema
  where_ $ reactionPipelineId reaction ==. pipelineId pipeline
  return reaction

getWorkflows :: Query (Pipeline Expr, ListTable Expr (Reaction Expr))
getWorkflows = do
    pipeline <- each pipelineSchema 
    reactions <- many $ reactionsForPipeline pipeline
    return (pipeline, reactions)

getWorkflow :: PipelineId -> Query (Pipeline Expr, ListTable Expr (Reaction Expr))
getWorkflow pId = do
    pipeline <- getPipelineById pId
    reactions <- many $ reactionsForPipeline pipeline
    return (pipeline, reactions)

getWorkflowsByUser :: UserId -> Query (Pipeline Expr, ListTable Expr (Reaction Expr))
getWorkflowsByUser uid = do
    pipeline <- getPipelineByUserId uid
    reactions <- many $ reactionsForPipeline pipeline
    return (pipeline, reactions)