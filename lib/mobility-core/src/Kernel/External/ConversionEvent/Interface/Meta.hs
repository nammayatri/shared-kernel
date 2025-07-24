module Kernel.External.ConversionEvent.Interface.Meta where

import EulerHS.Prelude
import Kernel.External.ConversionEvent.Meta.Flow as Meta
import Kernel.External.ConversionEvent.Meta.Types
import Kernel.External.ConversionEvent.Types as Interface
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Utils.Logging (logDebug)

conversionEventApiCall ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  MetaConfig ->
  Interface.ConversionReqType ->
  m ()
conversionEventApiCall config req = do
  logDebug $ "Request came from the Interface"
  let userData =
        UserDataType
          { em = Nothing,
            ph = Nothing,
            fn = Nothing,
            ln = Nothing,
            db = Nothing,
            ge = Nothing,
            ct = Nothing,
            st = Nothing,
            zp = Nothing,
            country = Nothing
          }

  let apiReq =
        MetaConversionDataType
          { eventName = req.eventName,
            eventTime = 0,
            userData = userData,
            customData = Nothing,
            eventSourceUrl = Nothing,
            optOut = Nothing,
            eventId = Nothing,
            actionSource = Nothing
          }
  logDebug $ "Request sent from the Interface"
  Meta.conversionEventApiCall config [apiReq]
