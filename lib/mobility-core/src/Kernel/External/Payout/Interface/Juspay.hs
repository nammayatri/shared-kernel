{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Kernel.External.Payout.Interface.Juspay
  ( module Reexport,
    payoutOrderStatusWebhook,
    createPayoutOrder,
    payoutOrderStatus,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.External.Payout.Interface.Types
import Kernel.External.Payout.Juspay.Config as Reexport
import qualified Kernel.External.Payout.Juspay.Flow as Juspay
import qualified Kernel.External.Payout.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Juspay.Webhook as Juspay
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Servant hiding (throwError)

createPayoutOrder ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayConfig ->
  CreatePayoutOrderReq ->
  m CreatePayoutOrderResp
createPayoutOrder config req = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  orderReq <- mkCreatePayoutOrderReq req
  mkCreatePayoutOrderResp <$> Juspay.createPayoutOrder url apiKey merchantId orderReq
  where
    mkCreatePayoutOrderReq CreatePayoutOrderReq {..} = do
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
                            { webhookDetails = Nothing,
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

payoutOrderStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  JuspayConfig ->
  Text ->
  m PayoutOrderStatusResp
payoutOrderStatus config orderId' = do
  let url = config.url
      merchantId = config.merchantId
  apiKey <- decrypt config.apiKey
  mkPayoutOrderStatusResp <$> Juspay.payoutOrderStatus url apiKey merchantId orderId'
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

mkWebhookOrderStatusPayoutResp :: Juspay.PayoutInfo -> OrderStatusPayoutResp
mkWebhookOrderStatusPayoutResp payoutInfo =
  parsePayoutWebhook
  where
    parsePayoutWebhook =
      OrderStatusPayoutResp
        { payoutOrderId = payoutInfo.id,
          payoutStatus = payoutInfo.status,
          orderType = payoutInfo.orderType,
          merchantOrderId = payoutInfo.merchantOrderId,
          merchantCustomerId = Just payoutInfo.merchantCustomerId,
          amount = realToFrac payoutInfo.amount,
          createdAt = readMaybe (T.unpack payoutInfo.createdAt) :: Maybe UTCTime,
          updatedAt = readMaybe (T.unpack payoutInfo.updatedAt) :: Maybe UTCTime
        }
