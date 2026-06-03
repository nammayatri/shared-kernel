module Kernel.External.ChallanSearch.Signzy.API where

import EulerHS.Types (EulerClient, client)
import Kernel.External.ChallanSearch.Signzy.Types as CST
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

type SignzyChallanSearchAPI =
  "api" :> "v3" :> "vehicle" :> "challan-search"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] CST.ChallanSearchReq
    :> Post '[JSON] CST.ChallanSearchResp

challanSearchClient :: Maybe Text -> CST.ChallanSearchReq -> EulerClient CST.ChallanSearchResp
challanSearchClient = client (Proxy :: Proxy SignzyChallanSearchAPI)

challanSearch :: (CoreMetrics m, MonadFlow m, Log m, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> CST.ChallanSearchReq -> m CST.ChallanSearchResp
challanSearch url apiKey req = do
  callAPI url (challanSearchClient (Just apiKey) req) "signzyChallanSearch" (Proxy :: Proxy SignzyChallanSearchAPI)
    >>= fromEitherM (ExternalAPICallError (Just "SIGNZY_CHALLAN_SEARCH_ERROR") url)
