module Kernel.External.PartnerSdk.Interface.Aarokya where

import Kernel.External.Encryption
import qualified Kernel.External.PartnerSdk.Aarokya.Flow as Aarokya
import qualified Kernel.External.PartnerSdk.Aarokya.Types as AarokyaTypes
import Kernel.External.PartnerSdk.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common

generateToken ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  AarokyaTypes.AarokyaSdkConfig ->
  GenerateTokenReq ->
  m GenerateTokenResp
generateToken config req = do
  apiKey <- decrypt config.apiKey
  let aarokyaReq = toAarokyaTokenRequest config req
  resp <- Aarokya.generateToken config.url apiKey aarokyaReq
  pure $ fromAarokyaTokenResponse resp

toAarokyaTokenRequest :: AarokyaTypes.AarokyaSdkConfig -> GenerateTokenReq -> AarokyaTypes.AarokyaTokenRequest
toAarokyaTokenRequest config req =
  AarokyaTypes.AarokyaTokenRequest
    { phone_country_code = req.phoneCountryCode,
      phone_number = req.phoneNumber,
      platform_id = config.platformId,
      dl_number = req.dlNumber
    }

fromAarokyaTokenResponse :: AarokyaTypes.AarokyaTokenResponse -> GenerateTokenResp
fromAarokyaTokenResponse resp =
  GenerateTokenResp
    { accessToken = resp.access_token
    }
