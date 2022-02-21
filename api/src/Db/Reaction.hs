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
import Rel8 (Column, DBEq, DBType, Expr, Insert (Insert, returning), JSONBEncoded (JSONBEncoded), Name, OnConflict (DoNothing), Query, ReadShow (ReadShow), Rel8able, Result, Returning (Projection), TableSchema (TableSchema, columns, name, schema), each, into, lit, nextval, onConflict, returning, rows, unsafeCastExpr, values, where_, (==.))

import Core.Reaction
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId)

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
