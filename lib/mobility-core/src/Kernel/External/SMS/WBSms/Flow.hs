{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.WBSms.Flow where

import EulerHS.Prelude
import qualified Kernel.External.SMS.WBSms.API as API
import Kernel.External.SMS.WBSms.Types (SubmitSms (..), SubmitSmsRes (..))
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  SubmitSms ->
  m SubmitSmsRes
submitSms url params = do
  callAPI url (API.wbSmsAPIClient params) "WBSms SMS API" API.wbSmsConnectAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

sendOTPApi ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  m SubmitSmsRes
sendOTPApi url passkey tempId wbSmsTemplate phoneNumber = do
  submitSms
    url
    SubmitSms
      { mobile = phoneNumber,
        message = wbSmsTemplate,
        templateid = tempId,
        passkey = passkey
      }
