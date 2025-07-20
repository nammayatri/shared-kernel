{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Kapture.Flow
  ( createTicketAPI,
    updateTicketAPI,
    addAndUpdateKaptureCustomer,
    kaptureEncryption,
    kapturePullTicket,
    kaptureGetTicket,
  )
where

import EulerHS.Types as Euler
import qualified Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (callAPI, fromEitherM)
import Servant hiding (throwError)

type KaptureCreateTicketAPI =
  "add-ticket-from-other-source.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Kapture.CreateTicketReq
    :> Post '[JSON] Kapture.CreateTicketResp

type KaptureUpdateTicketAPI =
  "update-ticket-from-other-source.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> Header "x-api-key" Text
    :> Header "x-api-type" Text
    :> ReqBody '[JSON] Kapture.UpdateTicketReq
    :> Post '[JSON] Kapture.UpdateTicketResp

createTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Kapture.CreateTicketReq ->
  m Kapture.CreateTicketResp
createTicketAPI url version auth req = do
  let eulerClient = Euler.client (Proxy @KaptureCreateTicketAPI)
  callAPI url (eulerClient version (Just auth) req) "createTicketAPI" (Proxy @KaptureCreateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call create ticket API: " <> show err)

updateTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Kapture.UpdateTicketReq ->
  m Kapture.UpdateTicketResp
updateTicketAPI url version auth req = do
  let eulerClient = Euler.client (Proxy @KaptureUpdateTicketAPI)
  callAPI url (eulerClient version (Just auth) (Just auth) (Just "TICKET") req) "updateTicketAPI" (Proxy @KaptureUpdateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call update ticket API: " <> show err)

type AddAndUpdateKaptureCustomerAPI =
  "add-update-customer-from-other-source.html"
    :> Header "Authorization" Text
    :> Header "x-api-key" Text
    :> Header "x-api-type" Text
    :> ReqBody '[JSON] [Kapture.KaptureCustomerReq]
    :> Post '[JSON] Kapture.KaptureCustomerResp

addAndUpdateKaptureCustomer ::
  (Metrics.CoreMetrics m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  Kapture.KaptureCustomerReq ->
  m Kapture.KaptureCustomerResp
addAndUpdateKaptureCustomer url apiKey req = do
  let eulerClient = Euler.client (Proxy @AddAndUpdateKaptureCustomerAPI) (Just apiKey) (Just apiKey) (Just "TICKET") [req]
  callAPI url eulerClient "add-and-update-kapture-customer" (Proxy @AddAndUpdateKaptureCustomerAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call add and update kapture customer API: " <> show err)

type KaptureEncryptionAPI =
  "customer-code-encryption"
    :> QueryParam "customer_code" Text
    :> QueryParam "encryption_key" Text
    :> Get '[JSON] Kapture.KaptureEncryptionResp

kaptureEncryption ::
  (Metrics.CoreMetrics m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  Text ->
  m Kapture.KaptureEncryptionResp
kaptureEncryption url customerCode encryptionKey = do
  let eulerClient = Euler.client (Proxy @KaptureEncryptionAPI) (Just customerCode) (Just encryptionKey)
  callAPI url eulerClient "kapture-encryption" (Proxy @KaptureEncryptionAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call kapture encryption API: " <> show err)

type KapturePullTicketAPI =
  "select-ticket-between-start-and-end-dates.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> Header "x-api-key" Text
    :> Header "x-api-type" Text
    :> ReqBody '[JSON] [Kapture.KapturePullTicketReq]
    :> Post '[JSON] Kapture.KapturePullTicketResp

kapturePullTicket ::
  (Metrics.CoreMetrics m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  Kapture.KapturePullTicketReq ->
  m Kapture.KapturePullTicketResp
kapturePullTicket url apiKey req = do
  let eulerClient = Euler.client (Proxy @KapturePullTicketAPI) "v.2.0" (Just apiKey) (Just apiKey) (Just "TICKET") [req]
  callAPI url eulerClient "kapturePullTicketAPI" (Proxy @KapturePullTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call select ticket API: " <> show err)

type KaptureGetTicketAPI =
  "search-ticket-by-ticket-id.html"
    :> Capture "version" Text
    :> Header "Authorization" Text
    :> Header "x-api-key" Text
    :> Header "x-api-type" Text
    :> ReqBody '[JSON] Kapture.GetTicketReq
    :> Post '[JSON] [Kapture.GetTicketResp]

kaptureGetTicket ::
  (Metrics.CoreMetrics m, MonadFlow m) =>
  BaseUrl ->
  Text ->
  Kapture.GetTicketReq ->
  m [Kapture.GetTicketResp]
kaptureGetTicket url apiKey req = do
  let eulerClient = Euler.client (Proxy @KaptureGetTicketAPI) "v.2.0" (Just apiKey) (Just apiKey) (Just "TICKET") req
  callAPI url eulerClient "kaptureGetTicket" (Proxy @KaptureGetTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call get ticket API: " <> show err)
