{-# LANGUAGE DeriveGeneric #-}

module User where

import GHC.Generics (Generic)
import Servant.Auth.JWT
-- import Servant.Auth.Server as SAS
import Data.Aeson (FromJSON, ToJSON)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Read, Generic)


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
