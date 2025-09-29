{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.TH where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified Language.Haskell.TH as TH
import Servant (FromHttpApiData (..), ToHttpApiData (..))

-- | A set of instances common for all identifier newtypes.
deriveIdentifierInstances :: TH.Name -> TH.Q [TH.Dec]
deriveIdentifierInstances name = do
  let tyQ = pure (TH.ConT name)
  [d|
    deriving stock instance Eq $tyQ

    deriving stock instance Ord $tyQ

    deriving newtype instance ToJSON $tyQ

    deriving newtype instance FromJSON $tyQ

    deriving newtype instance ToHttpApiData $tyQ

    deriving newtype instance FromHttpApiData $tyQ

    deriving newtype instance ToSchema $tyQ
    |]

mkHttpInstancesForEnum :: TH.Name -> TH.Q [TH.Dec]
mkHttpInstancesForEnum name = do
  fromInstance <- mkFromHttpInstanceForEnum name
  toInstance <- mkToHttpInstanceForEnum name
  pure $ fromInstance <> toInstance

mkFromHttpInstanceForEnum :: TH.Name -> TH.Q [TH.Dec]
mkFromHttpInstanceForEnum name = do
  let tyQ = pure (TH.ConT name)
  [d|
    instance FromHttpApiData $tyQ where
      parseUrlPiece = parseHeader . DT.encodeUtf8
      parseQueryParam = parseUrlPiece
      parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict
    |]

mkToHttpInstanceForEnum :: TH.Name -> TH.Q [TH.Dec]
mkToHttpInstanceForEnum name = do
  let tyQ = pure (TH.ConT name)
  [d|
    instance ToHttpApiData $tyQ where
      toUrlPiece = DT.decodeUtf8 . toHeader
      toQueryParam = toUrlPiece
      toHeader = BSL.toStrict . encode
    |]

-- This generates ToJSON and FromJSON instances that omit Nothing fields.
-- For example, if userEmail is Nothing, it won't appear in the JSON at all.
deriveJSONOmitNothing :: TH.Name -> TH.Q [TH.Dec]
deriveJSONOmitNothing name = do
  let tyQ = pure (TH.ConT name)
      options = [|defaultOptions {omitNothingFields = True}|]
  [d|
    instance ToJSON $tyQ where
      toJSON = genericToJSON $options
      toEncoding = genericToEncoding $options

    instance FromJSON $tyQ where
      parseJSON = genericParseJSON $options
    |]
