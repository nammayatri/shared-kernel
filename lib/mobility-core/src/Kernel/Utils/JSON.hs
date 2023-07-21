{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.JSON where

import Data.Aeson (Options (..), SumEncoding (ObjectWithSingleField, UntaggedValue), Value (..), camelTo2, defaultOptions)
import Data.HashMap.Strict (size, unions)
import Data.Text (pack, replace, toLower, toUpper, unpack)
import EulerHS.Prelude hiding (pack, unpack)
import Kernel.Utils.Text (recursiveStrip)

replaceUnderscores :: Text -> Text
replaceUnderscores = replace "_" "-"

replaceUnderscoresString :: String -> String
replaceUnderscoresString = unpack . replaceUnderscores . pack

constructorsWithHyphens :: Options
constructorsWithHyphens =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString
    }

constructorsWithHyphensUntagged :: Options
constructorsWithHyphensUntagged =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString,
      sumEncoding = UntaggedValue
    }

constructorsToLowerOptions :: Options
constructorsToLowerOptions =
  defaultOptions
    { constructorTagModifier = unpack . toLower . pack
    }

constructorsToUpperOptions :: Options
constructorsToUpperOptions =
  defaultOptions
    { constructorTagModifier = unpack . toUpper . pack
    }

constructorsWithHyphensToLowerOptions :: Options
constructorsWithHyphensToLowerOptions =
  defaultOptions
    { constructorTagModifier = unpack . replaceUnderscores . toLower . pack
    }

slashedRecordFields :: Options
slashedRecordFields =
  defaultOptions
    { fieldLabelModifier = unpack . replace "_" "/" . pack
    }

doubleQuotesRecordFields :: Options
doubleQuotesRecordFields =
  defaultOptions
    { fieldLabelModifier = unpack . replace "_" "\"" . pack
    }

uniteObjects :: [Value] -> Value
uniteObjects values =
  let result = unions objects
   in if size result == sumOfSizes
        then Object result
        else error ("duplication fields in " <> show values)
  where
    objects = map unwrapObject values
    unwrapObject (Object o) = o
    unwrapObject e = error ("expected Object, got " <> show e)
    sumOfSizes = sum $ map size objects

objectWithSingleFieldParsing :: (String -> String) -> Options
objectWithSingleFieldParsing constructorMapping =
  defaultOptions
    { sumEncoding = ObjectWithSingleField,
      constructorTagModifier = constructorMapping
    }

stripPrefixUnderscoreIfAny :: Options
stripPrefixUnderscoreIfAny =
  defaultOptions
    { fieldLabelModifier = recursiveStrip
    }

untaggedValue :: Options
untaggedValue =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

camelToSnakeCase :: String -> String
camelToSnakeCase = camelTo2 '_'

constructorsWithSnakeCase :: Options
constructorsWithSnakeCase =
  defaultOptions
    { fieldLabelModifier = camelToSnakeCase
    }

removeNullFields :: Options
removeNullFields =
  defaultOptions
    { omitNothingFields = True
    }

constructorsWithLowerCase :: Options
constructorsWithLowerCase =
  defaultOptions
    { fieldLabelModifier = unpack . toLower . pack
    }
