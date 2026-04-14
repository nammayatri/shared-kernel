module Kernel.External.Payout.Stripe.Flow where

import qualified Data.Text.Encoding as DT
-- import EulerHS.Types as Euler
import Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
-- import Kernel.Tools.Metrics.CoreMetrics as Metrics
-- import Kernel.Types.Common
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

callStripeAPI :: CallAPI m r api res
callStripeAPI url eulerClient description proxy = do
  callApiUnwrappingApiError (identity @StripeError) Nothing Nothing Nothing url eulerClient description proxy

mkBasicAuthData :: Text -> BasicAuthData
mkBasicAuthData apiKey =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 apiKey,
      basicAuthPassword = ""
    }
