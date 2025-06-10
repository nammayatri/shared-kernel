module Kernel.External.Insurance.Acko.Flow where

import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.External.Insurance.Acko.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type CreateInsuranceAPI =
  "product"
    :> Capture "partnerId" Text
    :> "policies"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] AckoInsuranceRequest
    :> Post '[JSON] AckoInsuranceResponse

createInsurance :: (Metrics.CoreMetrics m, EncFlow m r) => BaseUrl -> Text -> AckoInsuranceRequest -> m AckoInsuranceResponse
createInsurance url authHeader request = do
  let proxy = Proxy @CreateInsuranceAPI
      eulerClient = Euler.client proxy request.partner_id (Just authHeader) (Just "application/json") request
  callAckoAPI url eulerClient "create-insurance" proxy

callAckoAPI :: CallAPI' m api res res
callAckoAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> description <> " API: " <> show err)
