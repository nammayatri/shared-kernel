{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.GSTEInvoice.Types where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data GSTEInvoiceService = CharteredInfo | DigiGST
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''GSTEInvoiceService)
derivePersistField "GSTEInvoiceService"

instance FromJSON GSTEInvoiceService where
  parseJSON (String "CharteredInfo") = pure CharteredInfo
  parseJSON (String "DigiGST") = pure DigiGST
  parseJSON (String _) = parseFail "Expected \"CharteredInfo\" or \"DigiGST\""
  parseJSON e = typeMismatch "String" e

instance ToJSON GSTEInvoiceService where
  toJSON = String . show
