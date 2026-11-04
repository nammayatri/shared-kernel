{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | AWS KMS implementation of 'KmsDecryptor'.
--
-- Credentials come from the default AWS credential chain ('Amazonka.discover'), exactly like
-- the S3/SES code in the backend, so a pod that can already call those services only needs
-- @kms:Decrypt@ on the key added to its role.
module Kernel.Utils.Dhall.AwsKms
  ( mkAwsKmsDecryptor,
  )
where

import qualified Amazonka
import qualified Amazonka.KMS as KMS
import qualified Amazonka.KMS.Lens as KMS
import Control.Lens ((^.))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.IORef as IORef
import Kernel.Prelude
import Kernel.Utils.Dhall.KmsSecrets (KmsDecryptor, KmsSecretError (..))

decryptAttempts :: Int
decryptAttempts = 3

mkAwsKmsDecryptor :: IO KmsDecryptor
mkAwsKmsDecryptor = do
  envRef <- IORef.newIORef Nothing
  let getEnv = do
        cached <- IORef.readIORef envRef
        case cached of
          Just env -> pure env
          Nothing -> do
            env <- Amazonka.newEnv Amazonka.discover
            IORef.writeIORef envRef (Just env)
            pure env
  pure $ \ciphertext -> withRetries decryptAttempts $ do
    env <- getEnv
    let request = KMS.newDecrypt ciphertext
    response <- runResourceT $ Amazonka.send env request
    case response ^. KMS.decryptResponse_plaintext of
      Just plaintext -> pure plaintext
      Nothing -> throwIO $ KmsSecretError "AWS KMS Decrypt returned no plaintext"

withRetries :: Int -> IO a -> IO a
withRetries attempts action = go 1
  where
    go n = do
      result <- tryAny action
      case result of
        Right a -> pure a
        Left e
          | n >= attempts -> throwIO . KmsSecretError $ "AWS KMS Decrypt failed after " <> show attempts <> " attempts: " <> show e
          | otherwise -> threadDelay (n * 1000000) >> go (n + 1)
