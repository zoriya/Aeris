{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Core.User where
import GHC.Generics (Generic)
import Data.Text (Text)
import Rel8 (DBType, DBEq)
import Data.Int (Int64)
import Data.Aeson (ToJSON, FromJSON)


newtype UserId = UserId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show, Num, FromJSON, ToJSON)
  deriving stock (Generic)

data User = User 
  { userId        :: UserId
  , userName      :: Text
  , userSlug          :: Text
  } deriving stock (Generic)