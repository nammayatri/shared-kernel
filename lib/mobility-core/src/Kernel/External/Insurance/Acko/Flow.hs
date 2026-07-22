module Kernel.External.Insurance.Acko.Flow where

import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.External.Insurance.Acko.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Network.HTTP.Types as HTTP
import Servant hiding (throwError)
import Servant.Client (ClientError (..), ResponseF (responseStatusCode))

type CreateInsuranceAPI =
  "product"
    :> Capture "partnerId" Text
    :> "policies"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] AckoInsuranceRequest
    :> Post '[JSON] [AckoInsuranceResponse]

createInsurance :: (Metrics.CoreMetrics m, EncFlow m r, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> AckoInsuranceRequest -> m [AckoInsuranceResponse]
createInsurance url authHeader request = do
  let proxy = Proxy @CreateInsuranceAPI
      eulerClient = Euler.client proxy request.partner_id (Just authHeader) (Just "application/json") request
  callAckoAPI url eulerClient "create-insurance" proxy

callAckoAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callAckoAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM
      ( \err -> case err of
          FailureResponse _ resp
            | let code = HTTP.statusCode $ responseStatusCode resp,
              code >= 400 && code < 500 ->
              InvalidRequest $ "Failed to call " <> description <> " API (downstream 4xx rejection): " <> show err
          _ -> InternalError $ "Failed to call " <> description <> " API: " <> show err
      )
