module Kernel.External.Insurance.IffcoTokio.Types where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

data IffcoTokioConfig = IffcoTokioConfig
  { url :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    masterPolicyClient :: Text,
    insurancePlan :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HomeDeclarationReq = HomeDeclarationReq
  { insuredAddress :: Text,
    insuredEmail :: Text,
    insuredMobile :: Text,
    insuredName :: Text,
    -- | Date in MM/DD/YYYY format as required by IFFCO Tokio
    invoiceDate :: Text,
    -- | Correlation ID used to match the async response back to the caller
    invoiceRequestNumber :: Text,
    ewCommencesOn :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HomeDeclarationResp = HomeDeclarationResp
  { certificateNumber :: Text,
    declarationId :: Text,
    status :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
