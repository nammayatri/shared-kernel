{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.SAP.Flow
  ( fetchSAPToken,
    postJournalEntry,
  )
where

import qualified EulerHS.Types as ET
import Kernel.External.Encryption
import Kernel.External.SAP.API
import Kernel.External.SAP.Config
import Kernel.External.SAP.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

fetchSAPToken ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPServiceConfig ->
  m SAPTokenResp
fetchSAPToken config = do
  credentials <- decrypt config.sapAuthCredentials
  let eulerClient = ET.client sapTokenAPI
  callSAPAPI
    config.sapAuthUrl
    ( eulerClient
        (Just "client_credentials")
        (Just "jwt")
        (Just $ "Basic " <> credentials)
    )
    "sapTokenAuth"
    sapTokenAPI

postJournalEntry ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SAPServiceConfig ->
  Text ->
  SAPJournalRequest ->
  m SAPJournalResponse
postJournalEntry config accessToken req = do
  let xmlBody = renderJournalRequestXml req
      eulerClient = ET.client sapJournalPostAPI
  callSAPAPI
    config.sapApiUrl
    ( eulerClient
        (Just $ "Bearer " <> accessToken)
        xmlBody
    )
    "sapJournalPost"
    sapJournalPostAPI

callSAPAPI :: CallAPI m r api a
callSAPAPI =
  callApiUnwrappingApiError
    (identity @SAPError)
    Nothing
    (Just "SAP_API_ERROR")
    Nothing
