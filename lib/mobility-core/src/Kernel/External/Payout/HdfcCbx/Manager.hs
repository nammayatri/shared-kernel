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
  ( prepareHdfcCbxHttpManagers,
    prepareHdfcCbxHttpManagerFromPem,
  )
where

import qualified Data.Default.Class as Default
import qualified Data.HashMap.Strict as HMS
import qualified Data.PEM as PEM
import qualified Data.Text.Encoding as TE
import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509Store
import Kernel.External.Encryption (decrypt)
import Kernel.External.Payout.HdfcCbx.Config (HdfcCbxConfig, hdfcManagerKey)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

-- | A manager for every HDFC CBX config the deployment holds, keyed as the call sites expect.
--
-- Takes the configs rather than reading them: where they are stored is the caller's business,
-- the same shape as 'Kernel.Utils.Servant.SignatureAuth.prepareAuthManagers', which is handed
-- the subscribers the app loaded.
--
-- A config whose material will not load is logged and skipped. One bad certificate should fail
-- its own merchant's payouts when they are attempted, not stop the app from starting.
prepareHdfcCbxHttpManagers ::
  (EncFlow m r) =>
  -- | timeout, milliseconds
  Int ->
  [HdfcCbxConfig] ->
  m (HMS.HashMap Text Http.ManagerSettings)
prepareHdfcCbxHttpManagers timeout configs = do
  managers <- forM configs \cfg -> do
    -- Only the private key is encrypted; the certificate and the CA bundle are public material.
    clientKeyPem <- decrypt cfg.clientKeyPem
    let managerKey = hdfcManagerKey cfg
    case prepareHdfcCbxHttpManagerFromPem timeout managerKey cfg.clientCertPem clientKeyPem cfg.caBundlePem of
      Left err -> do
        logError $ "HDFC CBX manager " <> managerKey <> " was not built, payouts on it will fail: " <> err
        pure HMS.empty
      Right manager -> pure manager
  pure $ HMS.unions managers

-- | Manager settings presenting our client certificate, verifying the bank's server against a
-- supplied CA bundle, registered under a caller-supplied key.
--
-- The material comes as PEM text rather than file paths because it is stored in the merchant
-- service config beside the rest of the bank's credentials: the certificate is whitelisted
-- against a CBX domain, so it belongs with the domain it was issued for. That also lets one
-- deployment serve several cities on different certificates -- each gets its own manager, its
-- own connection pool, and presents its own certificate.
--
-- 'Left' carries what failed. The caller knows which merchant the material belongs to, and a
-- bare 'Nothing' would strand that at exactly the moment it is needed.
prepareHdfcCbxHttpManagerFromPem ::
  -- | timeout, milliseconds
  Int ->
  -- | key to register the manager under; see 'Config.hdfcManagerKey'
  Text ->
  -- | client certificate chain, PEM
  Text ->
  -- | client private key, PEM
  Text ->
  -- | CA bundle used to verify the bank's server certificate, PEM
  Text ->
  Either Text (HMS.HashMap Text Http.ManagerSettings)
prepareHdfcCbxHttpManagerFromPem timeout managerKey certPem keyPem caPem = do
  cred <-
    annotate "client certificate/key" $
      TLS.credentialLoadX509FromMemory (TE.encodeUtf8 certPem) (TE.encodeUtf8 keyPem)
  store <- caStoreFromPem caPem
  pure . HMS.singleton managerKey $ managerSettings timeout cred store

-- | A certificate store from a PEM bundle. The bundle may hold several certificates -- root plus
-- intermediates -- and verifying the bank's chain can need any of them, so all are loaded.
caStoreFromPem :: Text -> Either Text X509Store.CertificateStore
caStoreFromPem caPem = do
  pems <- annotate "CA bundle" . PEM.pemParseBS $ TE.encodeUtf8 caPem
  when (null pems) $ Left "CA bundle: no PEM blocks found"
  certs <- traverse decodeOne pems
  pure $ X509Store.makeCertificateStore certs
  where
    decodeOne = annotate "CA bundle" . X509.decodeSignedCertificate . PEM.pemContent

-- | Prefix a decode failure with what was being decoded. 'first' in this prelude is the tuple
-- one from Control.Arrow, not Bifunctor's, so the mapping is spelled out.
annotate :: Text -> Either String a -> Either Text a
annotate ctx = either (\e -> Left $ ctx <> ": " <> toText e) Right

managerSettings :: Int -> TLS.Credential -> X509Store.CertificateStore -> Http.ManagerSettings
managerSettings timeout cred store =
  let shared =
        Default.def
          { TLS.sharedCredentials = TLS.Credentials [cred],
            TLS.sharedCAStore = store
          }
      -- TLS 1.2 pinned: HDFC's gateway (an F5) aborts a 1.3 handshake with
      -- "bad record mac". Confirmed against UAT on 2026-08-31: these exact
      -- parameters complete the handshake; the previous defaults did not.
      supported =
        Default.def
          { TLS.supportedCiphers = TLS.ciphersuite_default,
            TLS.supportedVersions = [TLS.TLS12]
          }
      -- hs-tls presents a client certificate only through this hook.
      -- sharedCredentials is not consulted on the client side, so without the
      -- hook the handshake offers no certificate at all and mTLS fails.
      hooks =
        Default.def
          { TLS.onCertificateRequest = \_ -> pure (Just cred)
          }
      clientParams host =
        (TLS.defaultParamsClient host "")
          { TLS.clientSupported = supported,
            TLS.clientShared = shared,
            TLS.clientHooks = hooks
          }
      tlsSettings = Conn.TLSSettings (clientParams "")
   in (HttpTLS.mkManagerSettings tlsSettings Nothing)
        { Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)
        }
