{-# OPTIONS_GHC -Wno-missing-signatures #-}

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

mobileNumber = ExactLength 10 `And` star digit

mobileCountryCode = LengthInRange 2 4 `And` ("+" <> star digit)

mobileIndianCode :: Regex
mobileIndianCode = "+91"

name = star latinOrSpace
