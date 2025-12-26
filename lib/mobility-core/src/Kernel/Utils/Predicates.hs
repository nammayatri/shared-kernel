{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Predicates where

import Data.List (singleton)
import Kernel.Prelude
import Kernel.Types.Beckn.Country (Country (..))
import Kernel.Types.Predicate

digit, latinUC, latinLC, latin, alphanum, latinOrSpace, latinWithSymbols :: Regex
digit = charRange '0' '9'
latinUC = charRange 'A' 'Z'
latinLC = charRange 'a' 'z'
latin = latinUC \/ latinLC
alphanum = latin \/ digit
latinOrSpace = latin \/ " "
latinWithSymbols = latinOrSpace \/ basicSpecialSymbols

mobileNumber :: LengthInRange `And` Regex
mobileNumber = LengthInRange 8 15 `And` star digit

mobileCountryCode :: LengthInRange `And` Regex
mobileCountryCode = LengthInRange 2 4 `And` ("+" <> star digit)

fullMobilePhone :: LengthInRange `And` Regex
fullMobilePhone = LengthInRange 12 14 `And` ("+" <> star digit)

mobileIndianCode :: LengthInRange `And` Regex
mobileIndianCode = LengthInRange 3 3 `And` "+91" -- Added redundant LengthInRange to have same types for refactoring

getMobileNumberPredicate :: Country -> (LengthInRange `And` Regex)
getMobileNumberPredicate country
  | country == India = indianMobileNumber
  | country == Finland = finnishMobileNumber
  | otherwise = mobileNumber

getMobileCountryCodePredicate :: Country -> (LengthInRange `And` Regex)
getMobileCountryCodePredicate country
  | country == India = mobileIndianCode
  | country == Finland = finnishCountryCode
  | otherwise = mobileCountryCode

getCountryMobileCode :: Country -> Text
getCountryMobileCode country =
  case country of
    India -> "+91"
    Finland -> "+358"
    _ -> "+91"

name :: Regex
name = star latinOrSpace

nameWithNumber :: Regex
nameWithNumber = star $ alphanum \/ " "

inputName :: Regex
inputName = star latinWithSymbols

basicSpecialSymbols :: Regex
basicSpecialSymbols =
  unions $ map (fromString . singleton) "!#,.$%&'*+-/=?^_`{|}~()"

indianMobileNumber :: LengthInRange `And` Regex
indianMobileNumber = LengthInRange 10 10 `And` (charRange '6' '9' <> star digit)

finnishMobileNumber :: LengthInRange `And` Regex
finnishMobileNumber = LengthInRange 9 12 `And` star digit

finnishCountryCode :: LengthInRange `And` Regex
finnishCountryCode = LengthInRange 4 4 `And` "+358"

plus :: Regex -> Regex
plus r = r <> star r

-- sublocal#1.sublocal#2@subdomain-1.subdomain-2.zone
email :: LengthInRange `And` Regex
email = LengthInRange 5 254 `And` (localPart <> "@" <> domain)
  where
    -- not allowed dots: in the start, in the end, two or more dots in a row
    sublocal = plus (alphanum \/ specialSymbols)
    localPart = sublocal <> star ("." <> sublocal)
    subdomain = alphanum \/ (alphanum <> star (alphanum \/ "-") <> alphanum)
    subdomains = subdomain <> star ("." <> subdomain)
    zone = latin <> plus latin
    domain = subdomains <> "." <> zone

    specialSymbols :: Regex
    specialSymbols = unions $ map (fromString . singleton) "!#$%&'*+-/=?^_`{|}~"
