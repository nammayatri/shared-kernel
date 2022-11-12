{-# LANGUAGE DerivingVia #-}

module Beckn.External.GoogleTranslate.Types where

import Beckn.Prelude
import Beckn.Utils.JSON

data TranslateResp = TranslateResp
  { _data :: Translations,
    _error :: Maybe TranslateError
  }
  deriving (Generic, ToSchema)

instance FromJSON TranslateResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TranslateResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype TranslateError = TranslateError
  { code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype Translations = Translation
  { translations :: [TranslatedText]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype TranslatedText = TranslatedText
  { translatedText :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)