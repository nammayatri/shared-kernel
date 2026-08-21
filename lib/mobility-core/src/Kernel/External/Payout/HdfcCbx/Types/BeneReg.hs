{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

-- | Wire types for @cbx-nodal-beneReg@.
--
-- Whether registration is mandatory before a beneficiary can be paid, and whether it is
-- per rail, is still unconfirmed with HDFC. The types exist so the call can be wired
-- without a second round of schema work when the answer arrives.
module Kernel.External.Payout.HdfcCbx.Types.BeneReg where

import Kernel.External.Payout.HdfcCbx.Types.Payment (CdFlag)
import Kernel.Prelude

data CbxBeneRegReq = CbxBeneRegReq
  { clientcode :: Text,
    groupid :: Text,
    iduser :: Text,
    -- | Beneficiary code we assign; becomes @code@ on the payment transaction.
    code :: Text,
    -- | 25
    accno :: Text,
    -- | 11
    ifsc :: Text,
    -- | 200, max 40 on NEFT and RTGS
    name :: Text,
    -- | per-rail registration, if required
    cdflag :: Maybe CdFlag,
    email :: Maybe Text,
    bankname :: Maybe Text,
    branch :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CbxBeneRegResp = CbxBeneRegResp
  { clientcode :: Maybe Text,
    groupid :: Maybe Text,
    code :: Maybe Text,
    codstatus :: Maybe Text,
    message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
