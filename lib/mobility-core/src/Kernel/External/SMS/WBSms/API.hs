{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.WBSms.API where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.External.SMS.WBSms.Types (SubmitSms (..), SubmitSmsRes (..))
import Kernel.Utils.Servant.HTML (HTML)
import Servant hiding (throwError)

type WBSmsConnectAPI =
  "send_sms_yatri_sathi.php"
    :> ReqBody '[FormUrlEncoded] SubmitSms
    :> Post '[HTML] SubmitSmsRes

wbSmsConnectAPI :: Proxy WBSmsConnectAPI
wbSmsConnectAPI = Proxy

wbSmsAPIClient :: SubmitSms -> ET.EulerClient SubmitSmsRes
wbSmsAPIClient = ET.client wbSmsConnectAPI
