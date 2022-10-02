module Beckn.Storage.Hedis.AppPrefixes where

import Beckn.Prelude
import Beckn.Storage.Hedis.Config

appBackendPrefix :: KeyModifierFunc
appBackendPrefix = ("app-backend:" <>)

becknTransportPrefix :: KeyModifierFunc
becknTransportPrefix = ("beckn-transport:" <>)

publicTransportBapPrefix :: KeyModifierFunc
publicTransportBapPrefix = ("public-transport-bap:" <>)
