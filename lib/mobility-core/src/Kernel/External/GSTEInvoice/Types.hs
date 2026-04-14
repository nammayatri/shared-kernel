{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.GSTEInvoice.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data GSTEInvoiceService = CharteredInfo
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''GSTEInvoiceService)
derivePersistField "GSTEInvoiceService"

instance FromJSON GSTEInvoiceService where
  parseJSON (String "CharteredInfo") = pure CharteredInfo
  parseJSON (String _) = parseFail "Expected \"CharteredInfo\""
  parseJSON e = typeMismatch "String" e

instance ToJSON GSTEInvoiceService where
  toJSON = String . show
