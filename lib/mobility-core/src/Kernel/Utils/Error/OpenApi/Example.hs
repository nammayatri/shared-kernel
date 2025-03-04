module Kernel.Utils.Error.OpenApi.Example where

import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Time.Clock
import Data.Typeable (typeRep)
import Kernel.Prelude
import qualified Network.HTTP.Types as HTTP

class OpenApiExample a where
  mkOpenApiExample :: Int -> a
  default mkOpenApiExample :: (Typeable a, IsString a) => Int -> a
  mkOpenApiExample i = do
    let typeName = headToLower $ show $ typeRep (Proxy @a)
    fromString $ typeName <> "#" <> show i
    where
      headToLower :: String -> String
      headToLower (s : xs) = C.toLower s : xs
      headToLower [] = []

instance OpenApiExample Text

instance OpenApiExample String

instance OpenApiExample BS.ByteString

instance OpenApiExample HTTP.HeaderName

instance OpenApiExample HTTP.Header where
  mkOpenApiExample i = (mkOpenApiExample i, mkOpenApiExample i)

instance OpenApiExample UTCTime where
  mkOpenApiExample i = UTCTime {utctDay = toEnum i, utctDayTime = 0}

instance OpenApiExample Int where
  mkOpenApiExample = identity

instance {-# OVERLAPPABLE #-} OpenApiExample a => OpenApiExample [a] where
  mkOpenApiExample i = [mkOpenApiExample i]

instance OpenApiExample a => OpenApiExample (Maybe a) where
  mkOpenApiExample i = Just $ mkOpenApiExample i
