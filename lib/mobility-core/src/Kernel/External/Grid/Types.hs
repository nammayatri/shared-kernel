{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

-- | Types for the Juspay Grid decisions API (@POST /v1/decisions@).
--
-- Grid answers a set of typed questions (choice / noul / score) about free-text context and images.
-- Nothing here is use-case specific: callers build their own 'GridQuestions' and interpret the answers.
module Kernel.External.Grid.Types
  ( -- * Config
    GridCfg (..),
    GridAuthHeader (..),
    prepareGridHttpManager,
    gridHttpManagerKey,

    -- * Request
    GridDecisionReq (..),
    QuestionKey (..),
    GridQuestions (..),
    mkGridQuestions,
    mkGridQuestionsWithPhotoQuality,
    questionKeys,
    GridQuestion (..),
    questionInstructions,
    GridImage (..),
    ImageMime (..),
    mimeText,
    mkGridImageFromBase64,
    normalizeIdentifier,

    -- * Response
    GridDecisionResp (..),
    GridUsage (..),
    GridAnswer (..),
    ChoiceAnswer (..),
    NoulAnswer (..),
    ScoreAnswer (..),
    decodeAnswer,
    decodeAnswers,
    getChoice,
    getNoul,
    getScore,

    -- * Built-in questions
    photoQualityKey,
    photoQualityQuestion,
    getPhotoQuality,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.Key as AK
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.Char (isAlphaNum, isAscii)
import qualified Data.HashMap.Strict as HMS
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.Prelude
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import qualified Text.Show

-- * Config

data GridAuthHeader
  = -- | @Authorization: Bearer <key>@ (default)
    AuthorizationBearer
  | -- | @x-litellm-api-key: <key>@, the scheme declared in the LiteLLM OpenAPI spec
    XLiteLLMApiKey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GridCfg = GridCfg
  { url :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text,
    authHeader :: Maybe GridAuthHeader,
    model :: Text,
    -- | Upper bound on the decoded size of each image, checked before calling Grid.
    maxImageBytes :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Vision-model calls are slow; consumers register this manager with a suitable timeout (ms).
prepareGridHttpManager :: Int -> HMS.HashMap Text Http.ManagerSettings
prepareGridHttpManager timeout =
  HMS.singleton gridHttpManagerKey $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

gridHttpManagerKey :: Text
gridHttpManagerKey = "grid-http-manager"

-- * Request

data GridDecisionReq = GridDecisionReq
  { model :: Text,
    -- | Free-text context for the model. Never splice raw user input into it, see 'normalizeIdentifier'.
    state :: Text,
    images :: [GridImage],
    questions :: GridQuestions
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- Not derived: images are large base64 blobs and may contain PII.
instance Show GridDecisionReq where
  show r =
    "GridDecisionReq{model=" <> show r.model
      <> ", images="
      <> show (length r.images)
      <> ", questions="
      <> show (questionKeys r.questions)
      <> "}"

-- | Key of a question; the same key is used for the answer in the response.
newtype QuestionKey = QuestionKey {getQuestionKey :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype GridQuestions = GridQuestions (Map QuestionKey GridQuestion)
  deriving stock (Generic)
  deriving newtype (Show, Eq, ToJSON)

mkGridQuestions :: [(QuestionKey, GridQuestion)] -> GridQuestions
mkGridQuestions = GridQuestions . M.fromList

questionKeys :: GridQuestions -> [QuestionKey]
questionKeys (GridQuestions qs) = M.keys qs

data GridQuestion
  = -- | Pick exactly one option. Options are @(key, description)@; the answer is one of the keys.
    ChoiceQ Text (NonEmpty (Text, Text))
  | -- | Yes/no question. The answer is P(yes) in [0, 1].
    NoulQ Text
  | -- | Ordinal scale, worst to best. The answer is an expected score in [0, levels - 1].
    ScoreQ Text (NonEmpty Text)
  deriving stock (Show, Eq, Generic)

questionInstructions :: GridQuestion -> Text
questionInstructions = \case
  ChoiceQ i _ -> i
  NoulQ i -> i
  ScoreQ i _ -> i

-- Hand-written: "criteria" is an object for choice, an array for score and absent for noul.
instance ToJSON GridQuestion where
  toJSON = \case
    ChoiceQ instructions options ->
      object
        [ "type" .= ("choice" :: Text),
          "instructions" .= instructions,
          "criteria" .= object [AK.fromText k .= v | (k, v) <- NE.toList options]
        ]
    NoulQ instructions ->
      object
        [ "type" .= ("noul" :: Text),
          "instructions" .= instructions
        ]
    ScoreQ instructions levels ->
      object
        [ "type" .= ("score" :: Text),
          "instructions" .= instructions,
          "criteria" .= NE.toList levels
        ]

data ImageMime = ImageJpeg | ImagePng | ImageWebp
  deriving stock (Show, Eq, Generic)

mimeText :: ImageMime -> Text
mimeText = \case
  ImageJpeg -> "image/jpeg"
  ImagePng -> "image/png"
  ImageWebp -> "image/webp"

-- | Raw base64 without the @data:...;base64,@ prefix; the prefix is added on encoding.
data GridImage = GridImageBase64
  { mime :: ImageMime,
    base64Data :: Text
  }
  deriving stock (Eq, Generic)

instance ToJSON GridImage where
  toJSON (GridImageBase64 m b) = String ("data:" <> mimeText m <> ";base64," <> b)

instance Show GridImage where
  show (GridImageBase64 m b) = "GridImage(" <> show m <> ", " <> show (T.length b) <> " b64 chars)"

-- | Validates a base64 image (with or without a data-URI prefix): it must decode, be JPEG/PNG/WebP
-- by magic bytes, and be no larger than the given limit.
mkGridImageFromBase64 :: Maybe Int -> Text -> Either Text GridImage
mkGridImageFromBase64 mbMaxBytes raw = do
  let b64 = T.strip $ maybe raw snd (dataUriPayload raw)
  bytes <- either (const $ Left "Image is not valid base64") Right $ Base64.decode (encodeUtf8 b64)
  when (BS.null bytes) $ Left "Image is empty"
  whenJust mbMaxBytes $ \maxBytes ->
    when (BS.length bytes > maxBytes) $
      Left $ "Image is " <> show (BS.length bytes) <> " bytes, limit is " <> show maxBytes
  m <- maybe (Left "Unsupported image format, expected JPEG, PNG or WebP") Right $ detectMime bytes
  pure $ GridImageBase64 m b64
  where
    dataUriPayload t = case T.breakOn ";base64," t of
      (pre, rest) | "data:" `T.isPrefixOf` pre && not (T.null rest) -> Just (pre, T.drop (T.length ";base64,") rest)
      _ -> Nothing

detectMime :: BS.ByteString -> Maybe ImageMime
detectMime bs
  | BS.pack [0xFF, 0xD8, 0xFF] `BS.isPrefixOf` bs = Just ImageJpeg
  | BS.pack [0x89, 0x50, 0x4E, 0x47] `BS.isPrefixOf` bs = Just ImagePng
  | "RIFF" `BS.isPrefixOf` bs && "WEBP" `BS.isPrefixOf` BS.drop 8 bs = Just ImageWebp
  | otherwise = Nothing

-- | Normalizes a user-entered identifier (RC, DL, plate, ...) before it goes into prompt text:
-- keeps ASCII letters and digits only, uppercases, and checks the length is within @(minLen, maxLen)@.
-- Restricting to @[A-Z0-9]@ prevents prompt injection through the field.
normalizeIdentifier :: (Int, Int) -> Text -> Maybe Text
normalizeIdentifier (minLen, maxLen) raw =
  let t = T.toUpper (T.filter (\c -> isAscii c && isAlphaNum c) raw)
   in if T.length t >= minLen && T.length t <= maxLen then Just t else Nothing

-- * Response

data GridDecisionResp = GridDecisionResp
  { -- | Resolved model, e.g. @xor-omni-e4b@ for the @xor-omni@ alias.
    model :: Maybe Text,
    -- | Raw answers, decoded per question with 'decodeAnswers' so one bad answer can't fail the rest.
    answers :: Map QuestionKey Value,
    usage :: Maybe GridUsage
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON GridDecisionResp where
  parseJSON = withObject "GridDecisionResp" $ \o ->
    GridDecisionResp
      <$> o .:? "model"
      <*> o .:? "answers" .!= M.empty
      <*> (o .:? "usage" <|> pure Nothing)

data GridUsage = GridUsage
  { inputTokens :: Maybe Int,
    outputTokens :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON GridUsage where
  parseJSON = withObject "GridUsage" $ \o ->
    GridUsage <$> o .:? "input_tokens" <*> o .:? "output_tokens"

data ChoiceAnswer = ChoiceAnswer
  { choice :: Text,
    confidence :: Maybe Double,
    probabilities :: Map Text Double
  }
  deriving stock (Show, Eq, Generic)

newtype NoulAnswer = NoulAnswer
  { -- | Probability that the answer is "yes", in [0, 1].
    pYes :: Double
  }
  deriving stock (Show, Eq, Generic)

data ScoreAnswer = ScoreAnswer
  { -- | Expected score in [0, levels - 1]; not necessarily a whole number.
    score :: Double,
    confidence :: Maybe Double,
    -- | Keyed by level index as text ("0", "1", ...).
    probabilities :: Map Text Double
  }
  deriving stock (Show, Eq, Generic)

data GridAnswer
  = ChoiceA ChoiceAnswer
  | NoulA NoulAnswer
  | ScoreA ScoreAnswer
  deriving stock (Show, Eq, Generic)

-- | Decodes one answer against the question that was asked. Returns 'Nothing' if the answer's type
-- doesn't match the question, the choice isn't one of the offered keys, or a value is out of range.
decodeAnswer :: GridQuestion -> Value -> Maybe GridAnswer
decodeAnswer question = parseMaybe $
  withObject "GridAnswer" $ \o -> do
    answerType :: Text <- o .: "type"
    case (question, answerType) of
      (ChoiceQ _ options, "choice") -> do
        choice <- o .: "choice"
        unless (choice `elem` (fst <$> NE.toList options)) $ fail "unknown choice"
        confidence <- o .:? "confidence"
        probabilities <- o .:? "probabilities" .!= M.empty
        pure $ ChoiceA ChoiceAnswer {..}
      (NoulQ _, "noul") -> do
        pYes <- o .: "noul"
        inRange "noul" 0 1 pYes
        pure $ NoulA NoulAnswer {..}
      (ScoreQ _ levels, "score") -> do
        score <- o .: "score"
        inRange "score" 0 (fromIntegral (length levels - 1)) score
        confidence <- o .:? "confidence"
        probabilities <- o .:? "probabilities" .!= M.empty
        pure $ ScoreA ScoreAnswer {..}
      _ -> fail "answer type does not match question type"
  where
    inRange :: String -> Double -> Double -> Double -> Parser ()
    inRange name lo hi x = unless (x >= lo && x <= hi) $ fail (name <> " out of range")

-- | Decodes every answer for which a question was asked; missing or invalid answers are dropped.
decodeAnswers :: GridQuestions -> GridDecisionResp -> Map QuestionKey GridAnswer
decodeAnswers (GridQuestions qs) resp =
  M.mapMaybe identity $ M.intersectionWith decodeAnswer qs resp.answers

getChoice :: QuestionKey -> Map QuestionKey GridAnswer -> Maybe ChoiceAnswer
getChoice k answers = M.lookup k answers >>= \case ChoiceA a -> Just a; _ -> Nothing

getNoul :: QuestionKey -> Map QuestionKey GridAnswer -> Maybe NoulAnswer
getNoul k answers = M.lookup k answers >>= \case NoulA a -> Just a; _ -> Nothing

getScore :: QuestionKey -> Map QuestionKey GridAnswer -> Maybe ScoreAnswer
getScore k answers = M.lookup k answers >>= \case ScoreA a -> Just a; _ -> Nothing

-- | Like 'mkGridQuestions', plus the built-in 'photoQualityQuestion' under 'photoQualityKey'.
-- The built-in question wins if the caller also uses that key.
mkGridQuestionsWithPhotoQuality :: [(QuestionKey, GridQuestion)] -> GridQuestions
mkGridQuestionsWithPhotoQuality qs =
  GridQuestions . M.insert photoQualityKey photoQualityQuestion $ M.fromList qs

photoQualityKey :: QuestionKey
photoQualityKey = "photo_quality"

-- | Subject-agnostic image usability check, scored 0 (unusable) to 3 (good).
photoQualityQuestion :: GridQuestion
photoQualityQuestion =
  ScoreQ
    "How usable is this photo for verifying its main subject?"
    ( "unusable: blurred, too dark, cropped or the subject is not visible"
        :| [ "poor: the subject is visible but key details are hard to read",
             "ok: key details are readable with some effort",
             "good: sharp, well lit and every key detail is clearly readable"
           ]
    )

getPhotoQuality :: Map QuestionKey GridAnswer -> Maybe ScoreAnswer
getPhotoQuality = getScore photoQualityKey
