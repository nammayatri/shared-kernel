{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Utils.Dhall
  ( module Dhall,
    readDhallConfig,
    readDhallConfigDefault,
    customDecoder,
  )
where

import Data.Char (toUpper)
import qualified Data.Either.Validation as V
import qualified Data.Text.IO as TIO
import Dhall hiding (map)
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Kernel.Utils.Dhall.AwsKms (mkAwsKmsDecryptor)
import Kernel.Utils.Dhall.KmsSecrets (decryptKmsSecrets)
import Servant.Client (BaseUrl, Scheme, parseBaseUrl)
import Servant.Client.Core (InvalidBaseUrlException (..))
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)

-- | Reads config which lies under the given path.
readDhallConfig :: FromDhall b => FilePath -> IO b
readDhallConfig path = do
  txt <- TIO.readFile path
  parsed <- Dhall.Core.throws $ Dhall.Parser.exprFromText path txt
  resolved <- Dhall.Import.loadRelativeTo (takeDirectory path) Dhall.Import.UseSemanticCache parsed
  decryptor <- mkAwsKmsDecryptor
  decrypted <- decryptKmsSecrets decryptor resolved
  let decoder = auto
  expectedType <- case expected decoder of
    V.Success x -> pure x
    V.Failure e -> throwM e
  _ <- Dhall.Core.throws $ Dhall.TypeCheck.typeOf (Dhall.Core.Annot decrypted expectedType)
  Dhall.Core.throws . toMonadic . extract decoder $ Dhall.Core.normalize decrypted

-- | Reads config with a given type env. Gets application name as the second argument.
-- E.g. if @appname@ is "mock-provider-backend" the function first looks into "MOCK_PROVIDER_BACKEND_CONFIG_PATH"
-- env variable, if it's not set, it tries to read config from "./config/mock-provider-backend.dhall"
readDhallConfigDefault :: FromDhall b => String -> IO b
readDhallConfigDefault appname = do
  fname <- fromMaybe defCfgPath <$> lookupEnv envVarName
  readDhallConfig fname
  where
    defCfgPath = "./dhall-configs/dev/" ++ appname ++ ".dhall"
    envVarName = map norm appname ++ "_CONFIG_PATH"
    norm '-' = '_'
    norm c = toUpper c

-----------------------------------------------------

instance {-# OVERLAPS #-} Num a => FromDhall a where
  autoWith inn = fmap fromInteger (autoWith inn :: Decoder Integer)

-- Use the instance from Dhall itself
-- instance FromDhall Word16 where
--   autoWith inn = fmap fromIntegral (autoWith inn :: Decoder Natural)

deriving instance FromDhall Scheme

deriving instance FromDhall T.PoolConfig

deriving instance FromDhall T.PostgresConfig

deriving instance FromDhall T.RedisConfig

instance FromDhall BaseUrl where
  autoWith = customDecoder showBaseUrlErr parseBaseUrl . autoWith
    where
      showBaseUrlErr :: SomeException -> Text
      showBaseUrlErr e = case fromException e of
        Just (InvalidBaseUrlException msg) -> toText msg
        Nothing -> "Some unknown error: " <> show e

customDecoder :: (a1 -> Text) -> (t -> Either a1 a2) -> Decoder t -> Decoder a2
customDecoder ifErr parser Decoder {..} =
  Decoder
    { extract = \x -> fromMonadic do
        txt <- toMonadic (extract x)
        parser txt
          & either (toMonadic . extractError . ifErr) pure,
      ..
    }
