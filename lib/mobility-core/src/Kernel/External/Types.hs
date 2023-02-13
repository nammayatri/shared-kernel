{-# LANGUAGE DerivingVia #-}

module Kernel.External.Types where

import Data.OpenApi
import EulerHS.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (Showable))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

data Language
  = ENGLISH
  | HINDI
  | KANNADA
  | TAMIL
  | MALAYALAM
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)
  deriving (PrettyShow) via Showable Language

instance FromHttpApiData Language where
  parseUrlPiece "en" = pure ENGLISH
  parseUrlPiece "hi" = pure HINDI
  parseUrlPiece "kn" = pure KANNADA
  parseUrlPiece "ml" = pure MALAYALAM
  parseUrlPiece "ta" = pure TAMIL
  parseUrlPiece _ = Left "Unable to parse Language"

instance ToHttpApiData Language where
  toUrlPiece ENGLISH = "en"
  toUrlPiece HINDI = "hi"
  toUrlPiece KANNADA = "kn"
  toUrlPiece MALAYALAM = "ml"
  toUrlPiece TAMIL = "ta"
