module MultiModalVehicleType where

import Kernel.External.MultiModal.Interface.Types (GeneralVehicleType (..))
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTP
import Kernel.External.MultiModal.Utils (legVehicleType)
import Kernel.Prelude
import Test.Tasty
import Test.Tasty.HUnit

multiModalVehicleTypeTests :: TestTree
multiModalVehicleTypeTests =
  testGroup
    "OTP leg vehicle type"
    [ testCase "SHARED_CAB agency TAXI leg is Bus" $
        legVehicleType OTP.ModeTAXI (Just "chennai_shared_cab:SHARED_CAB") @?= Bus,
      testCase "other agency TAXI leg is Unspecified" $
        legVehicleType OTP.ModeTAXI (Just "chennai_bus:MTC") @?= Unspecified,
      testCase "SHARED_CAB agency BUS leg is Bus" $
        legVehicleType OTP.ModeBUS (Just "chennai_shared_cab:SHARED_CAB") @?= Bus,
      testCase "TAXI leg without agency is Unspecified" $
        legVehicleType OTP.ModeTAXI Nothing @?= Unspecified
    ]
