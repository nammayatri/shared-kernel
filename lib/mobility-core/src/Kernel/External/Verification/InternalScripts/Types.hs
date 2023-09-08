{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.InternalScripts.Types where

import Data.Aeson
import EulerHS.Prelude
import Kernel.Prelude

newtype FaceVerificationCfg = FaceVerificationCfg
  { url :: BaseUrl
  }
  deriving (Generic, ToJSON, FromJSON, Eq, Show)

data FaceType = REAL_FACE | FAKE_FACE | UNKNOWN
  deriving (Generic, FromJSON, Show, ToJSON)

data FaceValidationReq = FaceValidationReq
  { file :: Text,
    brisqueFeatures :: [Double]
  }
  deriving (Generic, ToJSON)

data FaceValidationRes = FaceValidationRes
  { faceType :: FaceType,
    score :: Double,
    predictionCost :: Double
  }
  deriving (Generic, FromJSON, Show, ToJSON)
