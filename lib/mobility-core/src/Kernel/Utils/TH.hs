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

import Data.Aeson (Options (..), defaultOptions)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import Kernel.Utils.JSON
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

-- | Generate ToJSON/FromJSON instances that omit Nothing fields from JSON output.
-- | Usage: $(deriveJSONOmitNothing ''User)
-- | Example: User{name="john", email=Nothing} -> {"name":"john"}
deriveJSONOmitNothing :: TH.Name -> TH.Q [TH.Dec]
deriveJSONOmitNothing name = do
  let tyQ = pure (TH.ConT name)
      options = [|defaultOptions {omitNothingFields = True}|]
  [d|
    instance ToJSON $tyQ where
      toJSON = genericToJSON $options

    instance FromJSON $tyQ where
      parseJSON = genericParseJSON $options
    |]

-- | Generate ToJSON/FromJSON instances with custom Aeson Options.
-- | Usage: $(deriveJSONWithOptions ''User [|removeNullFields|])
-- | Example: User{name="john", email=Nothing} -> {"name":"john"}
deriveJSONWithOptions :: TH.Name -> TH.Q TH.Exp -> TH.Q [TH.Dec]
deriveJSONWithOptions name optionsExp = do
  let tyQ = pure (TH.ConT name)
  [d|
    instance ToJSON $tyQ where
      toJSON = genericToJSON $optionsExp

    instance FromJSON $tyQ where
      parseJSON = genericParseJSON $optionsExp
    |]

-- | Generate ToJSON/FromJSON instances with snake_case field names and omit Nothing fields.
-- | Usage: $(deriveJSONSnakeCase ''User)
-- | Example: User{userName="john", userEmail=Nothing} -> {"user_name":"john"}
deriveJSONSnakeCase :: TH.Name -> TH.Q [TH.Dec]
deriveJSONSnakeCase name = do
  let tyQ = pure (TH.ConT name)
      options = [|constructorsWithSnakeCase {omitNothingFields = True}|]
  [d|
    instance ToJSON $tyQ where
      toJSON = genericToJSON $options

    instance FromJSON $tyQ where
      parseJSON = genericParseJSON $options
    |]

-- | Generate ToJSON/FromJSON instances with lowercase field names and omit Nothing fields.
-- | Usage: $(deriveJSONLowerCase ''User)
-- | Example: User{UserName="john", UserEmail=Nothing} -> {"username":"john"}
deriveJSONLowerCase :: TH.Name -> TH.Q [TH.Dec]
deriveJSONLowerCase name = do
  let tyQ = pure (TH.ConT name)
      options = [|constructorsWithLowerCase {omitNothingFields = True}|]
  [d|
    instance ToJSON $tyQ where
      toJSON = genericToJSON $options

    instance FromJSON $tyQ where
      parseJSON = genericParseJSON $options
    |]
