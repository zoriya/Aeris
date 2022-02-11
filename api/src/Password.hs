{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Password where

import Rel8 ( DBEq, DBType )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Text ( Text, unpack )

import Crypto.Random ( MonadRandom(getRandomBytes) )
import Crypto.KDF.BCrypt
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteArray (Bytes, convert)

newtype HashedPassword = HashedPassword { getHashedPasswd :: Text }
    deriving newtype (Eq, Show, Read, DBEq, DBType, FromJSON, ToJSON)

newtype Password = Password { getPassword :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

-- TODO Check if the password meets minimum security requirements
toPassword :: Text -> Password
toPassword = Password

-- Convert from bytes to bytestring and decode from bytestring to Text
bytesToText :: Bytes -> Text
bytesToText = decodeUtf8 . convert

-- Text to bytestring and convert to bytes
textToBytes :: Text -> Bytes
textToBytes = convert . encodeUtf8

newSalt :: MonadIO m => m Bytes
newSalt = liftIO $ getRandomBytes 16

hashPassword' :: Password -> Bytes -> HashedPassword
hashPassword' (Password password) salt =
    let hash = bcrypt 10 salt (textToBytes password)
    in HashedPassword $ bytesToText hash


hashPassword'' :: MonadIO m => Password -> m HashedPassword
hashPassword'' password = hashPassword' password <$> newSalt

{--
hashPassword' :: Password -> HashedPassword
hashPassword' (Password p) =
    HashedPassword $ bytesToText hash
    where
        hash = hashPassword 10 $ B.pack $ unpack p
--}
validatePassword' :: Password -> HashedPassword -> Bool
validatePassword' (Password p) (HashedPassword hp) = validatePassword (textToBytes p) (textToBytes hp)