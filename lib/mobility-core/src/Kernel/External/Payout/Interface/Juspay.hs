{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Payout.Interface.Juspay
  ( module Reexport,
    payoutOrderStatusWebhook,
    createPayoutOrder,
    payoutOrderStatus,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.External.Payout.Interface.Types
import Kernel.External.Payout.Juspay.Config as Reexport
import qualified Kernel.External.Payout.Juspay.Flow as Juspay
import qualified Kernel.External.Payout.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Juspay.Webhook as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Logging (logDebug)
import Servant hiding (throwError)

createPayoutOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  JuspayConfig ->
  Maybe Text ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder config mRoutingId req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  orderReq <- mkCreatePayoutOrderReq req
  mkCreatePayoutOrderResp <$> Juspay.createPayoutOrder url apiKey merchantId mRoutingId orderReq
  where
    mkCreatePayoutOrderReq CreatePayoutOrderReq {..} = do
      webhookDetails <- case isDynamicWebhookRequired of
        True -> Just <$> mkDynamicWebhookDetails
        False -> pure Nothing
      return $
        Juspay.CreatePayoutOrderReq
          { amount = realToFrac amount,
            fulfillments =
              Just
                [ Juspay.PayoutOrderReqFulfillment
                    { amount = realToFrac amount,
                      beneficiaryDetails =
                        Just $
                          Juspay.BeneficiaryDetails
                            { details =
                                Just
                                  Payout.AccountDetails
                                    { name = customerName,
                                      vpa = Just customerVpa,
                                      mobileNo = Just customerPhone
                                    },
                              detailsType = Just Juspay.UPI_ID
                            },
                      additionalInfo =
                        Just $
                          Payout.AdditionalInfo
                            { webhookDetails = webhookDetails,
                              remark = Just remark,
                              isRetriable = Nothing,
                              attemptThreshold = Nothing
                            },
                      udf1 = Nothing,
                      udf2 = Nothing,
                      udf3 = Nothing,
                      udf4 = Nothing,
                      udf5 = Nothing,
                      preferredMethodList = Nothing
                    }
                ],
            udf1 = Nothing,
            udf2 = Nothing,
            udf3 = Nothing,
            udf4 = Nothing,
            udf5 = Nothing,
            orderType = orderType,
            ..
          }
    mkCreatePayoutOrderResp Payout.PayoutOrderResp {..} = do
      CreatePayoutOrderResp
        { amount = realToFrac amount,
          ..
        }

    mkDynamicWebhookDetails = do
      appBaseUrl <- asks (.selfBaseUrl)
      password_ <- decrypt config.password
      dynamicWebhookUrl <- config.dynamicWebhookUrl & fromMaybeM (InvalidRequest "Dynamic webhook URL not found")
      let baseUrl = appBaseUrl {baseUrlPath = baseUrlPath appBaseUrl <> (T.unpack dynamicWebhookUrl)}
          url = showBaseUrl baseUrl
      let customHeaderList = [("X-MerchantId", config.merchantId)] :: [(Text, Text)]
          customHeader :: Text = TE.decodeUtf8 $ BL.toStrict $ A.encode $ HM.fromList customHeaderList
      logDebug $ "WebhookDetails: username: " <> show config.username <> " dynamicWebhookUrl: " <> show dynamicWebhookUrl <> " password: " <> show password_ <> " header: " <> show customHeader <> " webhookUrl: " <> show url
      return $ Payout.WebhookDetails {username = Just config.username, password = Just password_, customHeader = Just customHeader, url = Just url}

payoutOrderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayConfig ->
  Text ->
  Maybe Text ->
  Maybe Expand ->
  m PayoutOrderStatusResp
payoutOrderStatus config orderId' mRoutingId mbExpand = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkPayoutOrderStatusResp <$> Juspay.payoutOrderStatus url apiKey merchantId mRoutingId orderId' mbExpand
  where
    mkPayoutOrderStatusResp Payout.PayoutOrderResp {..} = do
      CreatePayoutOrderResp
        { amount = realToFrac amount,
          ..
        }

payoutOrderStatusWebhook ::
  EncFlow m r =>
  PayoutServiceConfig ->
  BasicAuthData ->
  A.Value ->
  m (Maybe OrderStatusPayoutResp)
payoutOrderStatusWebhook payoutConfig authData val = do
  response <- Juspay.payoutOrderStatusWebhook payoutConfig authData val
  return $ mkWebhookOrderStatusPayoutResp <$> response

mkWebhookOrderStatusPayoutResp :: Juspay.PayoutWebhookReq -> OrderStatusPayoutResp
mkWebhookOrderStatusPayoutResp payoutReq = case payoutReq.label of
  Just "ORDER" -> parsePayoutWebhook -- consuming only order level webhooks
  _ -> BadStatusResp
  where
    parsePayoutWebhook =
      OrderStatusPayoutResp
        { payoutOrderId = payoutReq.info.merchantOrderId,
          payoutStatus = payoutReq.info.status,
          orderType = payoutReq.info._type,
          merchantCustomerId = payoutReq.info.merchantCustomerId,
          amount = realToFrac payoutReq.info.amount,
          createdAt = payoutReq.info.createdAt,
          updatedAt = payoutReq.info.updatedAt
        }
