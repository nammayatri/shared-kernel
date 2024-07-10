{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payout.Juspay.Webhook where

import Data.Aeson.Types as DAT
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Payout.Juspay.Types as Juspay
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type JuspayPayoutWebhookAPI =
  "service" :> "juspay" :> "payout"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

payoutOrderStatusWebhook ::
  EncFlow m r =>
  PayoutServiceConfig ->
  BasicAuthData ->
  Value ->
  m (Maybe Juspay.PayoutInfo)
payoutOrderStatusWebhook payoutConfig authData val = do
  withLogTag "webhookPayoutOrderStatus" $ do
    let mResp = fromJSON val
    case mResp of
      DAT.Success (resp :: Juspay.PayoutWebhookReq) -> do
        void $ verifyAuth payoutConfig authData
        pure (Just resp.info)
      DAT.Error err -> do
        logInfo $ "PayoutOrderStatus Parsing failed :: " <> show err
        pure Nothing

verifyAuth ::
  EncFlow m r =>
  PayoutServiceConfig ->
  BasicAuthData ->
  m ()
verifyAuth config authData = do
  (username, password) <- case config of
    JuspayConfig cfg -> do
      cfgPassword <- decrypt cfg.password
      return (cfg.username, cfgPassword)
  unless (basicAuthUsername authData == DT.encodeUtf8 username && basicAuthPassword authData == DT.encodeUtf8 password) $
    throwError (InvalidRequest "INVALID_AUTHORIZATION_HEADER")
