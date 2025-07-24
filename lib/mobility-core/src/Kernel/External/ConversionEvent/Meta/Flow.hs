module Kernel.External.ConversionEvent.Meta.Flow where

import Data.Text as T
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.External.ConversionEvent.Meta.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant
import Servant.Client.Core (ClientError)

-- import Kernel.Utils.Logging (logDebug)

type MetaConversionApi =
  "graph.facebook.com"
    :> Capture "version" Text
    :> Capture "pixel_id" Text
    :> "events"
    :> QueryParam' '[Required] "access_token" Text
    :> ReqBody '[JSON] MetaConversionReqType
    :> Post '[JSON] ()

conversionEventApiCall ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  MetaConfig ->
  [MetaConversionDataType] ->
  m ()
conversionEventApiCall config events = do
  logDebug $ "Request came from the Meta"
  let req = MetaConversionReqType {data1 = events}
  let url = config.url
  logDebug $ "Request url sent from the Meta"
  _ <- callAPI url (callMetaConversionApi config.apiVersion config.pixelId config.accessToken req) "metaConversionEvent" metaConversionServiceAPI
  return ()
  where
    callMetaConversionApi = ET.client metaConversionServiceAPI

metaConversionServiceAPI :: Proxy MetaConversionApi
metaConversionServiceAPI = Proxy

metaConversionError :: BaseUrl -> ClientError -> ExternalAPICallError
metaConversionError = ExternalAPICallError (Just "META_CONVERSION_API_ERROR")
