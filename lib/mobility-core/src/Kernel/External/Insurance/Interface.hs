module Kernel.External.Insurance.Interface
  ( module Reexport,
    module Kernel.External.Insurance.Interface,
  )
where

import qualified Kernel.External.Insurance.Interface.Acko as Acko
import Kernel.External.Insurance.Interface.Types
import Kernel.External.Insurance.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

createInsurance ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  InsuranceConfig ->
  InsuranceRequest ->
  m InsuranceResponse
createInsurance serviceConfig req = case serviceConfig of
  AckoInsuranceConfig cfg -> Acko.createInsurance cfg req
