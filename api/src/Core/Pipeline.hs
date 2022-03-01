{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Pipeline where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode, Object)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded), ReadShow (ReadShow), DBEq)
import Servant (FromHttpApiData)

{--
data PipelineType = TwitterNewPost | TwitterNewFollower
    deriving stock (Generic, Read, Show)
    deriving (DBType) via ReadShow PipelineType
    deriving (FromJSON, ToJSON)
--}

-- newtype PipelineType = PipelineType { toText :: Text }
--     deriving stock (Generic, Read, Show)
--     deriving (DBType) via ReadShow PipelineType
--     deriving (FromJSON, ToJSON)

type PipelineType = Text

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

newtype PipelineParams
    = PipelineParams { toObject :: Object }
    deriving stock (Generic)
    deriving newtype (DBEq, Eq, Show, FromJSON, ToJSON)
    deriving (DBType) via JSONBEncoded PipelineParams
