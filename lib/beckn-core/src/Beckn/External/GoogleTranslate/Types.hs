{-# LANGUAGE DerivingVia #-}

module Beckn.External.GoogleTranslate.Types where

import Beckn.Prelude
import Beckn.Utils.JSON

type HasGoogleTranslate m r = (MonadReader r m, HasField "googleTranslateUrl" r BaseUrl, HasField "googleTranslateKey" r Text)
data TranslateResp = TranslateResp
  { _data :: Translations,
    _error :: Maybe TranslateError
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON TranslateResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TranslateResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype TranslateError = TranslateError
  { code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype Translations = Translation
  { translations :: [TranslatedText]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype TranslatedText = TranslatedText
  { translatedText :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)