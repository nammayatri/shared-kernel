{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

-- | The mutually-authenticated HTTP manager for HDFC CBX.
--
-- Registered on the FlowRuntime at startup alongside the other named managers, then
-- selected per call by 'Kernel.External.Payout.HdfcCbx.Config.tlsManagerKey'. Nothing else
-- in the payout path knows that the connection carries a client certificate.
--
-- The certificate here is deliberately separate from the JOSE signing key: one proves the
-- connection, the other proves the message. Conflating them is the usual way this
-- integration fails on its first live call.
module Kernel.External.Payout.HdfcCbx.Manager
  ( hdfcCbxHttpManagerKey,
    prepareHdfcCbxHttpManager,
  )
where

import qualified Data.Default.Class as Default
import qualified Data.HashMap.Strict as HMS
import qualified Data.X509.CertificateStore as X509Store
import Kernel.Prelude
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

hdfcCbxHttpManagerKey :: Text
hdfcCbxHttpManagerKey = "hdfc-cbx-http-manager"

-- | Builds manager settings that present our client certificate and verify theirs against
-- a supplied CA bundle.
--
-- Returns 'Nothing' when the material cannot be loaded, so a misconfigured deployment fails
-- at startup with a log line rather than at 2am on the first batch.
prepareHdfcCbxHttpManager ::
  -- | timeout, milliseconds
  Int ->
  -- | client certificate chain, PEM
  FilePath ->
  -- | client private key, PEM
  FilePath ->
  -- | CA bundle used to verify HDFC's server certificate
  FilePath ->
  IO (Maybe (HMS.HashMap Text Http.ManagerSettings))
prepareHdfcCbxHttpManager timeout certPath keyPath caPath = do
  credential <- TLS.credentialLoadX509 certPath keyPath
  mbStore <- X509Store.readCertificateStore caPath
  pure $ case (credential, mbStore) of
    (Right cred, Just store) -> Just . HMS.singleton hdfcCbxHttpManagerKey $ settings cred store
    _ -> Nothing
  where
    settings cred store =
      let shared =
            Default.def
              { TLS.sharedCredentials = TLS.Credentials [cred],
                TLS.sharedCAStore = store
              }
          supported = Default.def {TLS.supportedCiphers = TLS.ciphersuite_default}
          clientParams host =
            (TLS.defaultParamsClient host "")
              { TLS.clientSupported = supported,
                TLS.clientShared = shared
              }
          tlsSettings = Conn.TLSSettings (clientParams "")
       in (HttpTLS.mkManagerSettings tlsSettings Nothing)
            { Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)
            }
