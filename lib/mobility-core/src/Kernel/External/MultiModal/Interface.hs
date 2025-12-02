module Kernel.External.MultiModal.Interface
  ( module Reexport,
    getTransitRoutesProvided,
    getTransitRoutes,
  )
where

import Kernel.External.Maps.Google.Config as Reexport
import qualified Kernel.External.MultiModal.Interface.Google as Google
import qualified Kernel.External.MultiModal.Interface.OpenTripPlanner as OTP
import Kernel.External.MultiModal.Interface.Types as Reexport
import Kernel.External.MultiModal.OpenTripPlanner.Config as Reexport
import Kernel.External.MultiModal.Types
import Kernel.External.MultiModal.Utils as Reexport
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Servant.Client

-- To be used in future when we add another provider
-- mkNotProvidedError :: Text -> MultiModalService -> Text
-- mkNotProvidedError functionName serviceName = "Function " <> functionName <> " is not provided by service " <> show serviceName
--
-- throwNotProvidedError :: (MonadFlow m) => Text ->  MultiModalService -> m a
-- throwNotProvidedError =
--   (throwError . InternalError) ... mkNotProvidedError

getTransitRoutesProvided :: MultiModalService -> Bool
getTransitRoutesProvided = \case
  GoogleTransit -> True
  OTPTransit -> True

getTransitRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    HasKafkaProducer r,
    HasRequestId r,
    MonadReader r m
  ) =>
  Maybe Text ->
  MultiModalServiceConfig ->
  GetTransitRoutesReq ->
  m (Maybe MultiModalResponse)
getTransitRoutes entityId serviceConfig req = case serviceConfig of
  GoogleTransitConfig cfg -> Google.getTransitRoutes entityId cfg req
  OTPTransitConfig cfg -> OTP.getTransitRoutes cfg req
