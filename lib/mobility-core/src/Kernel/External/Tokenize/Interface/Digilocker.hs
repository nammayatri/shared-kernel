{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Tokenize.Interface.Digilocker where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Tokenize.Digilocker.Flow as DigilockerFlow
import qualified Kernel.External.Tokenize.Digilocker.Types as DigilockerTypes
import Kernel.External.Tokenize.Interface.Error
import qualified Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

tokenize ::
  ( CoreMetrics m,
    EncFlow m r,
    Log m
  ) =>
  DigilockerTypes.DigilockerTokenizeConfig ->
  InterfaceTypes.TokenizationReq ->
  m InterfaceTypes.TokenizationResp
tokenize config req = do
  logInfo "DigiLocker tokenize: Preparing tokenize request"
  digilockerReq <- makeDigilockerTokenizeRequest config req
  logDebug $ "DigiLocker tokenize: Request prepared - " <> show digilockerReq
  resp <- DigilockerFlow.tokenize config.url digilockerReq
  logInfo "DigiLocker tokenize: Processing response"
  makeDigilockerTokenizeResp resp
  where
    makeDigilockerTokenizeRequest :: EncFlow m r => DigilockerTypes.DigilockerTokenizeConfig -> InterfaceTypes.TokenizationReq -> m DigilockerTypes.DigilockerTokenizeRequest
    makeDigilockerTokenizeRequest DigilockerTypes.DigilockerTokenizeConfig {..} InterfaceTypes.TokenizationReq {..} = do
      clientSecret' <- decrypt clientSecret
      code' <- fromMaybeM (TokenNotFound "DigiLocker") code
      codeVerifier' <- fromMaybeM (TokenNotFound "DigiLocker") codeVerifier
      return $
        DigilockerTypes.DigilockerTokenizeRequest
          { grant_type = "authorization_code",
            code = code',
            client_id = clientId,
            client_secret = clientSecret',
            redirect_uri = redirectUri,
            code_verifier = codeVerifier'
          }
    makeDigilockerTokenizeResp DigilockerTypes.DigilockerTokenizeResponse {..} = do
      let expiresAt = consent_valid_till <&> (\validTill -> posixSecondsToUTCTime $ fromIntegral validTill)
      return $ InterfaceTypes.TokenizationResp {token = access_token, ..}
