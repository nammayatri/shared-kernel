 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Error.BaseError.HTTPError.FromResponse where

import qualified Data.Aeson as A
import EulerHS.Prelude
import Servant.Client (Response, ResponseF (Response))

class FromResponse e where
  fromResponse :: Response -> Maybe e

fromJsonResponse :: FromJSON a => Response -> Maybe a
fromJsonResponse (Response _ _ _ body) = A.decode body
