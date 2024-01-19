{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Payment.Juspay.Webhook where

import Control.Lens ((^.))
import Data.Aeson.Types as DAT
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude hiding ((^.))
import Kernel.External.Encryption
import Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type JuspayWebhookAPI =
  "service" :> "juspay" :> "payment"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

orderStatusWebhook ::
  EncFlow m r =>
  PaymentServiceConfig ->
  (Juspay.WebhookReq -> Text -> m AckResponse) ->
  BasicAuthData ->
  Value ->
  m (Maybe (Juspay.PaymentStatus, Juspay.OrderAndNotificationStatusContent))
orderStatusWebhook paymentConfig orderStatusHandler authData val = do
  withLogTag "webhookPaymentOrderStatus" $ do
    let respDump = encodeToText val
    let mResp = fromJSON val
    case mResp of
      DAT.Success (resp :: Juspay.WebhookReq) -> do
        void $ verifyAuth paymentConfig authData
        void $ orderStatusHandler resp respDump
        pure (Just (resp.event_name, resp.content))
      DAT.Error err -> do
        logInfo $ "OrderStatus Parsing failed :: " <> show err
        pure Nothing

verifyAuth ::
  EncFlow m r =>
  PaymentServiceConfig ->
  BasicAuthData ->
  m ()
verifyAuth config authData = do
  let (username, password) = case config of
        JuspayConfig cfg -> (cfg ^. #username, cfg ^. #password)

  cfgPassword <- decrypt password
  unless (basicAuthUsername authData == DT.encodeUtf8 username && basicAuthPassword authData == DT.encodeUtf8 cfgPassword) $
    throwError (InvalidRequest "INVALID_AUTHORIZATION_HEADER")
