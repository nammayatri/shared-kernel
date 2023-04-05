{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Predicates where

import Kernel.Prelude
import Kernel.Types.Predicate

digit, latinUC, latinLC, latin, alphanum, latinOrSpace :: Regex
digit = charRange '0' '9'
latinUC = charRange 'A' 'Z'
latinLC = charRange 'a' 'z'
latin = latinUC \/ latinLC
alphanum = latin \/ digit
latinOrSpace = latin \/ " "

mobileNumber :: ExactLength `And` Regex
mobileNumber = ExactLength 10 `And` star digit

mobileCountryCode :: LengthInRange `And` Regex
mobileCountryCode = LengthInRange 2 4 `And` ("+" <> star digit)

fullMobilePhone :: LengthInRange `And` Regex
fullMobilePhone = LengthInRange 12 14 `And` ("+" <> star digit)

mobileIndianCode :: Regex
mobileIndianCode = "+91"

name :: Regex
name = star latinOrSpace
