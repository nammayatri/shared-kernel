{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Types.JsonParam
  ( JsonList (..),
  )
where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import GHC.Generics
import Kernel.Prelude
import Servant.API

newtype JsonList a = JsonList {unJsonList :: [a]}
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromHttpApiData (JsonList a) where
  parseUrlPiece =
    BF.first T.pack
      . fmap JsonList
      . eitherDecode
      . BSL.fromStrict
      . DT.encodeUtf8

instance (ToJSON a) => ToHttpApiData (JsonList a) where
  toUrlPiece (JsonList xs) =
    DT.decodeUtf8 . BSL.toStrict $ encode xs
