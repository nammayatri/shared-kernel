{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.Interface.ImageDetectionTypes
  ( module Kernel.External.Verification.Interface.ImageDetectionTypes,
  )
where

import Data.Aeson (object, withObject, withText, (.:), (.:?), (.=))
import EulerHS.Prelude
import Kernel.Prelude

data FaceDetectionStatus = FaceDetected | NoFaces
  deriving stock (Show, Generic)

instance FromJSON FaceDetectionStatus where
  parseJSON = withText "FaceDetectionStatus" $ \case
    "success" -> pure FaceDetected
    "no_faces" -> pure NoFaces
    other -> fail $ "Unknown face detection status: " <> show other

instance ToJSON FaceDetectionStatus where
  toJSON FaceDetected = toJSON ("success" :: Text)
  toJSON NoFaces = toJSON ("no_faces" :: Text)

instance ToSchema FaceDetectionStatus

data FaceDetectionRecommendation = FullFace | PartialFace | RejectedFace
  deriving stock (Show, Generic)

instance FromJSON FaceDetectionRecommendation where
  parseJSON = withText "FaceDetectionRecommendation" $ \case
    "full" -> pure FullFace
    "partial" -> pure PartialFace
    "rejected" -> pure RejectedFace
    other -> fail $ "Unknown recommendation: " <> show other

instance ToJSON FaceDetectionRecommendation where
  toJSON FullFace = toJSON ("full" :: Text)
  toJSON PartialFace = toJSON ("partial" :: Text)
  toJSON RejectedFace = toJSON ("rejected" :: Text)

instance ToSchema FaceDetectionRecommendation

data FaceDetectionSummary = FaceDetectionSummary
  { status :: FaceDetectionStatus,
    fullFaces :: Maybe Int,
    partialFaces :: Maybe Int,
    rejectedFaces :: Maybe Int,
    total :: Maybe Int,
    recommendation :: Maybe FaceDetectionRecommendation,
    message :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON FaceDetectionSummary where
  parseJSON = withObject "FaceDetectionSummary" $ \o ->
    FaceDetectionSummary
      <$> o .: "status"
      <*> o .:? "full_faces"
      <*> o .:? "partial_faces"
      <*> o .:? "rejected_faces"
      <*> o .:? "total"
      <*> o .:? "recommendation"
      <*> o .:? "message"

instance ToJSON FaceDetectionSummary where
  toJSON FaceDetectionSummary {..} =
    object
      [ "status" .= status,
        "full_faces" .= fullFaces,
        "partial_faces" .= partialFaces,
        "rejected_faces" .= rejectedFaces,
        "total" .= total,
        "recommendation" .= recommendation,
        "message" .= message
      ]

instance ToSchema FaceDetectionSummary
