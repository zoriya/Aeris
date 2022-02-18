module Utils where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Types (Object, Value (String))

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]


lookupObj :: Object -> Text -> Maybe String
lookupObj obj key = case HM.lookup key obj of
                            Just (String x) -> Just . T.unpack $ x
                            _ -> Nothing