{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.HyperVerge where

import Kernel.External.Encryption as Common (EncFlow)
import qualified Kernel.External.Verification.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)

verifySdkResp ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeVerificationCfg ->
  InterfaceTypes.VerifySdkDataReq ->
  m InterfaceTypes.VerifySdkDataResp
verifySdkResp config req = do
  let hvReq = convertVerifySdkRespToHyperVergeReq req
  resp <- HyperVergeFlow.verifySdkResp config hvReq
  return $ convertHvSdkVerificationRespToVerifySdkResp resp
  where
    convertVerifySdkRespToHyperVergeReq :: InterfaceTypes.VerifySdkDataReq -> HyperVergeTypes.HyperVergeSdkVerificationReq
    convertVerifySdkRespToHyperVergeReq InterfaceTypes.VerifySdkDataReq {..} = HyperVergeTypes.HyperVergeSdkVerificationReq {..}

    convertHvSdkVerificationRespToVerifySdkResp :: HyperVergeTypes.HyperVergeSdkVerificationRes -> InterfaceTypes.VerifySdkDataResp
    convertHvSdkVerificationRespToVerifySdkResp HyperVergeTypes.HyperVergeSdkVerificationRes {..} =
      InterfaceTypes.VerifySdkDataResp
        { userDetails = join $ result <&> (.userDetails),
          status = join $ result <&> (.status),
          transactionId = join $ result <&> (.transactionId)
        }
