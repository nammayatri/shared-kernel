module Kernel.External.SocialSecurity.Interface
  ( module Reexport,
    module Kernel.External.SocialSecurity.Interface,
  )
where

import qualified Kernel.External.SocialSecurity.Interface.EShram as EShram
import Kernel.External.SocialSecurity.Interface.Types as Reexport
import Kernel.External.SocialSecurity.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

generateAuthToken ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  SocialSecurityConfig ->
  m AuthTokenResp
generateAuthToken serviceConfig = case serviceConfig of
  EShramSocialSecurityConfig cfg -> EShram.generateAuthToken cfg

getUnverifiedWorkers ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  SocialSecurityConfig ->
  Text ->
  UnverifiedWorkersReq ->
  m UnverifiedWorkersResp
getUnverifiedWorkers serviceConfig authToken req = case serviceConfig of
  EShramSocialSecurityConfig cfg -> EShram.getUnverifiedWorkers cfg authToken req

pushWorkerVerifications ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  SocialSecurityConfig ->
  Text ->
  [WorkerVerification] ->
  m PushVerificationResp
pushWorkerVerifications serviceConfig authToken verifications = case serviceConfig of
  EShramSocialSecurityConfig cfg -> EShram.pushWorkerVerifications cfg authToken verifications

insertMonthlyUtilization ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  SocialSecurityConfig ->
  Text ->
  [WorkerMonthlyUtilization] ->
  m MonthlyUtilizationResp
insertMonthlyUtilization serviceConfig authToken utilizations = case serviceConfig of
  EShramSocialSecurityConfig cfg -> EShram.insertMonthlyUtilization cfg authToken utilizations
