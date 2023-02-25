 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.TH where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Language.Haskell.TH as TH
import Servant (FromHttpApiData, ToHttpApiData)

-- | A set of instances common for all identifier newtypes.
deriveIdentifierInstances :: TH.Name -> TH.Q [TH.Dec]
deriveIdentifierInstances name = do
  let tyQ = pure (TH.ConT name)
  [d|
    deriving stock instance Eq $tyQ

    deriving stock instance Ord $tyQ

    deriving newtype instance ToJSON $tyQ

    deriving newtype instance FromJSON $tyQ

    deriving newtype instance ToHttpApiData $tyQ

    deriving newtype instance FromHttpApiData $tyQ

    deriving newtype instance ToSchema $tyQ
    |]
