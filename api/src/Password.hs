{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Password where

import Rel8 ( DBEq, DBType )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Text ( Text )

import Crypto.KDF.BCrypt
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype HashedPassword = HashedPassword { getHashedPasswd :: Text }
    deriving newtype (Eq, Show, Read, DBEq, DBType)

newtype Password = Password { getPassword :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

-- TODO Check if the password meets minimum security requirements
toPassword :: Text -> Password
toPassword = Password

bytesToText :: ByteString -> Text
bytesToText = decodeUtf8

textToBytes :: Text -> ByteString
textToBytes = encodeUtf8

validatePassword' :: Password -> HashedPassword -> Bool
validatePassword' (Password p) (HashedPassword hp) = validatePassword (textToBytes p) (textToBytes hp)