module Kernel.External.Insurance.Interface.Acko where

import EulerHS.Language (MonadFlow)
import Kernel.External.Encryption
import qualified Kernel.External.Insurance.Acko.Flow as Acko
import qualified Kernel.External.Insurance.Acko.Types as AckoTypes
import Kernel.External.Insurance.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Time (getCurrentTime)

createInsurance :: (Metrics.CoreMetrics m, EncFlow m r) => AckoTypes.AckoInsuranceConfig -> InsuranceRequest -> m InsuranceResponse
createInsurance config request = do
  let insuranceReq = toAckoInsuranceRequest request
  authHeader <- mkAuthHeader config
  resp <- Acko.createInsurance config.url authHeader insuranceReq
  return $ fromAckoInsuranceResponse resp

mkAuthHeader ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  AckoTypes.AckoInsuranceConfig ->
  m Text
mkAuthHeader config = do
  createdOn <- getCurrentTime
  secret <- decrypt config.apiKey
  return $ "username=" <> config.username <> " algorithm=key_auth created_on=" <> show createdOn <> " secret=" <> secret

---------------------------------------- Conversion functions ----------------------------------------

toAckoInsuranceRequest :: InsuranceRequest -> AckoTypes.AckoInsuranceRequest
toAckoInsuranceRequest req =
  AckoTypes.AckoInsuranceRequest
    { trip = convertTrip req.trip,
      category = req.category,
      customer = convertCustomer req.customer,
      plan_type = req.planType,
      reference_id = req.referenceId,
      plan = req.plan,
      partner_id = req.partnerId
    }

fromAckoInsuranceResponse :: AckoTypes.AckoInsuranceResponse -> InsuranceResponse
fromAckoInsuranceResponse resp =
  InsuranceResponse
    { policyId = resp.policy_id,
      referenceId = resp.reference_id,
      planType = Just resp.plan_type,
      startDate = resp.start_date,
      endDate = resp.end_date,
      policyNumber = resp.policy_number,
      certificatePdf = Just resp.certificate_pdf,
      certificateUrl = Just resp.certificate_url,
      person = Just $ map convertPersonResponse resp.person,
      premium = Just $ convertPremium resp.premium
    }

convertTrip :: Trip -> AckoTypes.Trip
convertTrip trip =
  AckoTypes.Trip
    { journey = map convertJourney trip.journey,
      end_date = trip.endDate,
      booking_id = trip.bookingId,
      start_date = trip.startDate,
      booking_date = trip.bookingDate
    }

convertJourney :: Journey -> AckoTypes.Journey
convertJourney journey =
  AckoTypes.Journey
    { mode = journey.mode,
      origin = convertLocation journey.origin,
      person = map convertPerson journey.person,
      destination = convertLocation journey.destination
    }

convertLocation :: Location -> AckoTypes.Location
convertLocation location =
  AckoTypes.Location
    { city = location.city
    }

convertPerson :: Person -> AckoTypes.Person
convertPerson person =
  AckoTypes.Person
    { insured = convertInsured person.insured
    }

convertInsured :: Insured -> AckoTypes.Insured
convertInsured insured =
  AckoTypes.Insured
    { name = insured.name,
      phone = insured.phone
    }

convertCustomer :: Customer -> AckoTypes.Customer
convertCustomer customer =
  AckoTypes.Customer
    { id = customer.id,
      name = customer.name,
      phone = customer.phone,
      state = customer.state
    }

convertPersonResponse :: AckoTypes.PersonResponse -> PersonResponse
convertPersonResponse personResp =
  PersonResponse
    { insured = convertInsuredResponse personResp.insured
    }

convertInsuredResponse :: AckoTypes.InsuredResponse -> InsuredResponse
convertInsuredResponse insuredResp =
  InsuredResponse
    { name = Just insuredResp.name,
      phone = Just insuredResp.phone,
      policyNumber = Just insuredResp.policy_number
    }

convertPremium :: AckoTypes.Premium -> Premium
convertPremium premium =
  Premium
    { amount = premium.amount,
      gst = Just premium.gst,
      sgst = Just premium.sgst,
      cgst = Just premium.cgst,
      igst = Just premium.igst,
      breakup = Just premium.breakup
    }
