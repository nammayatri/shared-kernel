{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Types where

import Data.OpenApi
import Database.Beam
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption (EncFlow)
import Kernel.Storage.Esqueleto (EsqDBFlow)
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Utils.GenericPretty (PrettyShow, Showable (Showable))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

data Language
  = ENGLISH
  | HINDI
  | KANNADA
  | TAMIL
  | MALAYALAM
  | BENGALI
  | FRENCH
  | TELUGU
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)
  deriving (PrettyShow) via Showable Language

$(mkBeamInstancesForEnumAndList ''Language)

instance FromHttpApiData Language where
  parseUrlPiece "en" = pure ENGLISH
  parseUrlPiece "hi" = pure HINDI
  parseUrlPiece "kn" = pure KANNADA
  parseUrlPiece "ml" = pure MALAYALAM
  parseUrlPiece "ta" = pure TAMIL
  parseUrlPiece "bn" = pure BENGALI
  parseUrlPiece "fr" = pure FRENCH
  parseUrlPiece "te" = pure TELUGU
  parseUrlPiece _ = Left "Unable to parse Language"

instance ToHttpApiData Language where
  toUrlPiece ENGLISH = "en"
  toUrlPiece HINDI = "hi"
  toUrlPiece KANNADA = "kn"
  toUrlPiece MALAYALAM = "ml"
  toUrlPiece TAMIL = "ta"
  toUrlPiece BENGALI = "bn"
  toUrlPiece FRENCH = "fr"
  toUrlPiece TELUGU = "te"

type ServiceFlow m r = (EncFlow m r, EsqDBFlow m r, CacheFlow m r)
