module HJ.JSON (JSON(..), parseJSON, encodeJSON, object, field, string, number, array) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Foldable (toList)

-- A tiny project-codec convenience layer. Aeson does all JSON parsing/encoding.
data JSON = Object [(String,JSON)] | Array [JSON] | String String | Number Double | Boolean Bool | Null deriving (Eq,Show)
parseJSON :: String -> Either String JSON
parseJSON = fmap fromValue . A.eitherDecodeStrict' . TE.encodeUtf8 . T.pack
encodeJSON :: JSON -> String
encodeJSON = T.unpack . TE.decodeUtf8 . BL.toStrict . A.encode . toValue
toValue :: JSON -> A.Value
toValue (Object xs) = A.object [K.fromString k A..= toValue v | (k,v)<-xs]
toValue (Array xs) = A.toJSON (map toValue xs)
toValue (String s) = A.String (T.pack s)
toValue (Number n) = A.Number (realToFrac n)
toValue (Boolean b) = A.Bool b
toValue Null = A.Null
fromValue :: A.Value -> JSON
fromValue (A.Object xs) = Object [(K.toString k,fromValue v) | (k,v)<-KM.toList xs]
fromValue (A.Array xs) = Array (map fromValue (toList xs))
fromValue (A.String s) = String (T.unpack s)
fromValue (A.Number n) = Number (realToFrac n)
fromValue (A.Bool b) = Boolean b
fromValue A.Null = Null
object :: JSON -> Either String [(String,JSON)]
object (Object xs) = Right xs
object _ = Left "Expected JSON object"
field :: String -> JSON -> Either String JSON
field k j = object j >>= maybe (Left ("Missing field: "++k)) Right . lookup k
string :: JSON -> Either String String
string (String s) = Right s
string _ = Left "Expected string"
number :: JSON -> Either String Double
number (Number n) | not (isNaN n || isInfinite n) = Right n
number _ = Left "Expected finite number"
array :: JSON -> Either String [JSON]
array (Array xs) = Right xs
array _ = Left "Expected array"
