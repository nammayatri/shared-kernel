module Kernel.Storage.Hedis.AppPrefixes where

import Kernel.Prelude
import Kernel.Storage.Hedis.Config

riderAppPrefix :: KeyModifierFunc
riderAppPrefix = ("app-backend:" <>)

staticOfferDriverAppPrefix :: KeyModifierFunc
staticOfferDriverAppPrefix = ("static-offer-driver-app:" <>)

publicTransportBapPrefix :: KeyModifierFunc
publicTransportBapPrefix = ("public-transport-bap:" <>)
