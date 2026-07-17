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
  basicToken <- decrypt config.basicToken
  let aarokyaReq = toAarokyaTokenRequest req
  resp <- Aarokya.generateToken config.url basicToken aarokyaReq
  pure $ fromAarokyaTokenResponse resp

toAarokyaTokenRequest :: GenerateTokenReq -> AarokyaTypes.AarokyaTokenRequest
toAarokyaTokenRequest req =
  AarokyaTypes.AarokyaTokenRequest
    { phone_country_code = req.phoneCountryCode,
      phone_number = req.phoneNumber,
      id_proof = toAarokyaIdProof req.idProof,
      dob = req.dob,
      address = req.address,
      gender = req.gender,
      first_name = req.firstName,
      last_name = req.lastName
    }

toAarokyaIdProof :: IdProof -> AarokyaTypes.AarokyaIdProof
toAarokyaIdProof p =
  AarokyaTypes.AarokyaIdProof
    { proof_type = p.proofType,
      number = p.number
    }

fromAarokyaTokenResponse :: AarokyaTypes.AarokyaTokenResponse -> GenerateTokenResp
fromAarokyaTokenResponse resp =
  GenerateTokenResp
    { accessToken = resp.access_token
    }

generateContributorToken ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  AarokyaTypes.AarokyaSdkConfig ->
  GenerateContributorTokenReq ->
  m GenerateContributorTokenResp
generateContributorToken config req = do
  basicToken <- decrypt config.basicToken
  let aarokyaReq = toAarokyaContributorTokenRequest req
  resp <- Aarokya.generateContributorToken config.url basicToken aarokyaReq
  pure $ fromAarokyaContributorTokenResponse resp

toAarokyaContributorTokenRequest :: GenerateContributorTokenReq -> AarokyaTypes.AarokyaContributorTokenRequest
toAarokyaContributorTokenRequest req =
  AarokyaTypes.AarokyaContributorTokenRequest
    { beneficiary_identifier = req.beneficiaryIdentifier,
      contribution_channel = req.contributionChannel,
      contributor_ref = toAarokyaContributorReference <$> req.contributorRef
    }

toAarokyaContributorReference :: ContributorRef -> AarokyaTypes.AarokyaContributorReference
toAarokyaContributorReference ref =
  AarokyaTypes.AarokyaContributorReference
    { type_ = ref.refType,
      value = ref.refValue
    }

fromAarokyaContributorTokenResponse :: AarokyaTypes.AarokyaContributorTokenResponse -> GenerateContributorTokenResp
fromAarokyaContributorTokenResponse resp =
  GenerateContributorTokenResp
    { contributorToken = resp.contributor_token
    }
