{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

import Rel8
    ( DBEq,
      DBType,
      (==.),
      unsafeCastExpr,
      nextval,
      each,
      where_,
      many,
      values,
      lit,
      Column,
      Expr,
      Rel8able,
      Query,
      Name,
      Result,
      TableSchema(..),
      Delete(..),
      Insert(..),
      OnConflict(DoNothing),
      Returning(NumberOfRowsAffected, Projection),
      ListTable )

import Core.Reaction
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId (PipelineId), pipelineSchema, Pipeline (pipelineId, pipelineUserId), getPipelineById, getPipelineByUserId)
import Core.User (UserId, User (User))
import Db.User (getUserById, UserDB)

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

getWorkflows :: Query (Pipeline Expr, ListTable Expr (Reaction Expr), UserDB Expr)
getWorkflows = do
    pipeline <- each pipelineSchema 
    reactions <- many $ reactionsForPipeline pipeline
    user <- getUserById $ pipelineUserId pipeline
    return (pipeline, reactions, user)

getWorkflow :: PipelineId -> Query (Pipeline Expr, ListTable Expr (Reaction Expr), UserDB Expr)
getWorkflow pId = do
    pipeline <- getPipelineById pId
    reactions <- many $ reactionsForPipeline pipeline
    user <- getUserById $ pipelineUserId pipeline
    return (pipeline, reactions, user)

getWorkflowsByUser :: UserId -> Query (Pipeline Expr, ListTable Expr (Reaction Expr))
getWorkflowsByUser uid = do
    pipeline <- getPipelineByUserId uid
    reactions <- many $ reactionsForPipeline pipeline
    return (pipeline, reactions)

deleteReaction :: ReactionId -> Delete Int64
deleteReaction rId =
    Delete
        { from = reactionSchema
        , using = pure()
        , deleteWhere = \_ row -> reactionId row ==. lit rId
        , returning = NumberOfRowsAffected
        }


deleteReactionsByPipelineId :: PipelineId  -> Delete Int64
deleteReactionsByPipelineId pId =
    Delete
        { from = reactionSchema
        , using = pure()
        , deleteWhere = \_ row -> reactionPipelineId row ==. lit pId
        , returning = NumberOfRowsAffected
        }