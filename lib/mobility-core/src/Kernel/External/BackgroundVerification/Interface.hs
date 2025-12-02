module Kernel.External.BackgroundVerification.Interface
  ( module Reexport,
    module Kernel.External.BackgroundVerification.Interface,
  )
where

import Kernel.External.BackgroundVerification.Interface.Checkr as Checkr
import Kernel.External.BackgroundVerification.Interface.Types as Reexport
import Kernel.External.BackgroundVerification.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

createCandidate ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  BackgroundVerificationServiceConfig ->
  CreateCandidateReq ->
  m CreateCandidateResp
createCandidate serviceConfig req = case serviceConfig of
  CheckrConfig cfg -> Checkr.createCandidate cfg req

createInvitation ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  BackgroundVerificationServiceConfig ->
  CreateInvitationReqI ->
  m CreateInvitationResp
createInvitation serviceConfig req = case serviceConfig of
  CheckrConfig cfg -> Checkr.createInvitation cfg req

getInvitation ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  BackgroundVerificationServiceConfig ->
  Text ->
  m GetInvitationResp
getInvitation serviceConfig invitationId = case serviceConfig of
  CheckrConfig cfg -> Checkr.getInvitation cfg invitationId

getReport ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  BackgroundVerificationServiceConfig ->
  Text ->
  m GetReportResp
getReport serviceConfig reportId = case serviceConfig of
  CheckrConfig cfg -> Checkr.getReport cfg reportId
