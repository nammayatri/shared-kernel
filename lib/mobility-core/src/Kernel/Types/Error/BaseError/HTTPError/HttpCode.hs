{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Error.BaseError.HTTPError.HttpCode where

import EulerHS.Prelude
import Servant.Server.Internal

data HttpCode
  = E400
  | E401
  | E402
  | E403
  | E404
  | E429
  | E500
  | E501
  | E503
  deriving (Show)

codeToHttpCode :: Int -> Maybe HttpCode
codeToHttpCode = \case
  400 -> Just E400
  401 -> Just E401
  402 -> Just E402
  403 -> Just E403
  404 -> Just E404
  429 -> Just E429
  500 -> Just E500
  501 -> Just E501
  503 -> Just E503
  _ -> Nothing

codeToHttpCodeWith500Default :: Int -> HttpCode
codeToHttpCodeWith500Default = fromMaybe E500 . codeToHttpCode

toServerError :: HttpCode -> ServerError
toServerError = \case
  E400 -> err400
  E401 -> err401
  E402 -> err402
  E403 -> err403
  E404 -> err404
  E429 ->
    ServerError
      { errHTTPCode = 429,
        errReasonPhrase = "Too Many Requests",
        errBody = "",
        errHeaders = []
      }
  E500 -> err500
  E501 -> err501
  E503 -> err503

isInternalError :: HttpCode -> Bool
isInternalError = \case
  E400 -> False
  E401 -> False
  E402 -> False
  E403 -> False
  E404 -> False
  E429 -> False
  E500 -> True
  E501 -> True
  E503 -> True
