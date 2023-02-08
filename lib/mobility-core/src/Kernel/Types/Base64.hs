{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Kernel.Types.Base64 where

import qualified Data.Aeson as A
import Data.Bifunctor (bimap)
import Data.ByteString
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.Dhall

newtype Base64 = Base64 ByteString
  deriving (Show, Eq)
  deriving newtype (PersistFieldSql)

instance PersistField Base64 where
  toPersistValue (Base64 t) = PersistText . decodeUtf8 $ Base64.encode t
  fromPersistValue (PersistText t) = bimap T.pack Base64 . Base64.decode $ encodeUtf8 t
  fromPersistValue x = Left $ "When trying to deserialize an Base64: expected PersistText, received: " <> T.pack (show x)

instance FromDhall Base64 where
  autoWith = customDecoder T.pack (fmap Base64 . Base64.decode . encodeUtf8 @Text) . autoWith

instance ToJSON Base64 where
  toJSON (Base64 bs) = A.String $ decodeUtf8 $ Base64.encode bs

instance FromJSON Base64 where
  parseJSON = A.withText "Base64" decodeBase64
    where
      decodeBase64 txt =
        Base64.decode (encodeUtf8 txt)
          & either fail (pure . Base64)

-- Use only for constant values in test code
instance IsString Base64 where
  fromString = Base64 . Base64.decodeLenient . encodeUtf8
