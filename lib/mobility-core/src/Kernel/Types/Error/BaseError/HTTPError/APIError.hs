module Kernel.Types.Error.BaseError.HTTPError.APIError
  ( module Kernel.Types.Error.BaseError.HTTPError.APIError,
    module Kernel.Types.Error.BaseError.HTTPError.HttpCode,
    module Kernel.Types.Error.BaseError,
  )
where

import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Types.Error.BaseError.HTTPError.HttpCode
import Control.Exception
import Data.Aeson (Value (Null))
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))

type IsAPIException e = (IsAPIError e, Exception e)

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text,
    errorPayload :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromResponse APIError where
  fromResponse = fromJsonResponse

class IsAPIError e where
  toPayload :: e -> Value
  toPayload _ = Null
