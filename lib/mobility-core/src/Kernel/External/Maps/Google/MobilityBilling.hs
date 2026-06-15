{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Maps.Google.MobilityBilling
  ( reportBillableEvent,
  )
where

import qualified Data.Aeson as A
import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client (Scheme (..))

type ReportBillableEventAPI =
  "v1:reportBillableEvent"
    :> MandatoryQueryParam "regionCode" Text
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "billableEventId" Text
    :> ReqBody '[JSON] A.Value
    :> Post '[JSON] A.Value

defaultMobilityBillingBaseUrl :: BaseUrl
defaultMobilityBillingBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "mobilitybilling.googleapis.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }

reportBillableEventClient :: Text -> Text -> Text -> A.Value -> EulerClient A.Value
reportBillableEventClient = client (Proxy :: Proxy ReportBillableEventAPI)

reportBillableEvent ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasRequestId r
  ) =>
  Maybe BaseUrl -> -- override base url (falls back to the default Google host)
  Text -> -- regionCode (CLDR/ISO 3166-1 alpha-2, e.g. "IN")
  Text -> -- Google API key
  Text -> -- billableEventId (the trip / ride id)
  m ()
reportBillableEvent mbBaseUrl regionCode apiKey billableEventId = do
  let baseUrl = fromMaybe defaultMobilityBillingBaseUrl mbBaseUrl
  result <-
    callAPI
      baseUrl
      (reportBillableEventClient regionCode apiKey billableEventId (A.object []))
      "reportBillableEvent"
      (Proxy :: Proxy ReportBillableEventAPI)
  case result of
    Right _ -> logInfo $ "Reported Google mobility billable event for trip " <> billableEventId
    Left err -> logError $ "Failed to report Google mobility billable event for trip " <> billableEventId <> ": " <> show err
