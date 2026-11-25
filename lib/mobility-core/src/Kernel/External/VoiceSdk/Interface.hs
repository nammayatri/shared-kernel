module Kernel.External.VoiceSdk.Interface
  ( module Reexport,
    module Kernel.External.VoiceSdk.Interface,
  )
where

import qualified Kernel.External.VoiceSdk.Interface.BreezeBuddy as BreezeBuddy
import Kernel.External.VoiceSdk.Interface.Types
import Kernel.External.VoiceSdk.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

createLead ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  VoiceSdkConfig ->
  CreateLeadReq ->
  m CreateLeadResp
createLead serviceConfig req = case serviceConfig of
  BreezeBuddyVoiceSdkConfig cfg -> BreezeBuddy.createLead cfg req

connect ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  VoiceSdkConfig ->
  ConnectReq ->
  m ConnectResp
connect serviceConfig req = case serviceConfig of
  BreezeBuddyVoiceSdkConfig cfg -> BreezeBuddy.connect cfg req
