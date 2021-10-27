{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventMessage (
    LineEventMessageType (..)
  , LineEventMessageMentionee (..)
  , LineEventMessageMention (..)
  , LineEventMessageEmoji (..)
  , LineEventMessage (..)
) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             defaultOptions, fieldLabelModifier,
                                             genericToJSON, object, (.:), (.:?),
                                             (.=))
import           Data.Aeson.Types           (Value (..), prependFailure,
                                             typeMismatch)
import           Data.Scientific
import qualified Data.Text                  as T
import           GHC.Generics

import           LBKiirotori.Internal.Utils (stripFirstToLowerLabeledOption)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects

data LineEventMessageType = LineEventMessageTypeText
    | LineEventMessageTypeImage
    | LineEventMessageTypeVideo
    | LineEventMessageTypeAudio
    | LineEventMessageTypeFile
    | LineEventMessageTypeLocation
    | LineEventMessageTypeSticker
    deriving (Eq, Show)

instance FromJSON LineEventMessageType where
    parseJSON (String "text")       = pure LineEventMessageTypeText
    parseJSON (String "image")      = pure LineEventMessageTypeImage
    parseJSON (String "video")      = pure LineEventMessageTypeVideo
    parseJSON (String "audio")      = pure LineEventMessageTypeAudio
    parseJSON (String "file")       = pure LineEventMessageTypeFile
    parseJSON (String "location")   = pure LineEventMessageTypeLocation
    parseJSON (String "sticker")    = pure LineEventMessageTypeSticker
    parseJSON invalid               = prependFailure "parsing LineEventMessageType failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventMessageType where
    toJSON LineEventMessageTypeText     = String "text"
    toJSON LineEventMessageTypeVideo    = String "image"
    toJSON LineEventMessageTypeAudio    = String "video"
    toJSON LineEventMessageTypeFile     = String "audio"
    toJSON LineEventMessageTypeLocation = String "location"
    toJSON LineEventMessageTypeSticker  = String "sticker"

data LineEventMessageMentionee = LineEventMessageMentionee {
    lemmMentioneeIndex  :: Scientific
  , lemmMentioneeLength :: Scientific
  , lemmMentioneeUserId :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageMentionee where
    parseJSON (Object v) = LineEventMessageMentionee
        <$> v .: "index"
        <*> v .: "length"
        <*> v .: "userId"
    parseJSON invalid = prependFailure "parsing LineEventMessageMentionee failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageMentionee where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 13

type LineEventMessageMentionees = [LineEventMessageMentionee]

newtype LineEventMessageMention = LineEventMessageMention {
    lemmMentionees :: LineEventMessageMentionees
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageMention where
    parseJSON (Object v) = LineEventMessageMention
        <$> v .: "mentionees"
    parseJSON invalid = prependFailure "parsing LineEventMessageMention failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageMention where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

data LineEventMessageEmoji = LineEventMessageEmoji {
    lemeIndex     :: Scientific
  , lemeLength    :: Scientific
  , lemeProductId :: T.Text
  , lemeEmojiId   :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageEmoji where
    parseJSON (Object v) = LineEventMessageEmoji
        <$> v .: "index"
        <*> v .: "length"
        <*> v .: "productId"
        <*> v .: "emojiId"
    parseJSON invalid = prependFailure "parsing LineEventMessageEmoji failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageEmoji where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

type LineEventMessageEmojis = [LineEventMessageEmoji]

data LineEventMessageContentProviderType = LineEventMessageContentProviderTypeLine
    | LineEventMessageContentProviderTypeExternal
    deriving (Eq, Show)

instance FromJSON LineEventMessageContentProviderType where
    parseJSON (String "line") = pure LineEventMessageContentProviderTypeLine
    parseJSON (String "external") = pure LineEventMessageContentProviderTypeExternal
    parseJSON invalid = prependFailure "parsing LineEventMessageContentProviderType failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventMessageContentProviderType where
    toJSON LineEventMessageContentProviderTypeLine     = String "line"
    toJSON LineEventMessageContentProviderTypeExternal = String "external"

data LineEventMessageContentProvider = LineEventMessageContentProvider {
    lemcpType               :: LineEventMessageContentProviderType
  , lemcpOriginalContentUrl :: Maybe T.Text
  , lemcpPreviewImageUrl    :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageContentProvider where
    parseJSON (Object v) = LineEventMessageContentProvider
        <$> v .: "type"
        <*> v .:? "originalContentUrl"
        <*> v .:? "previewImageUrl"
    parseJSON invalid = prependFailure "parsing LineEventMessageContentProvider failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageContentProvider where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 5

data LineEventMessageImageSet = LineEventMessageImageSet {
    lemiId    :: T.Text
  , lemiIndex :: Scientific
  , lemiTotal :: Scientific
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageImageSet where
    parseJSON (Object v) = LineEventMessageImageSet
        <$> v .: "id"
        <*> v .: "index"
        <*> v .: "total"

instance ToJSON LineEventMessageImageSet where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

data LineEventMessage = LineEventMessage {
    lemId              :: T.Text
  , lemType            :: LineEventMessageType
  , lemText            :: Maybe T.Text
  , lemEmojis          :: Maybe LineEventMessageEmojis
  , lemMention         :: Maybe LineEventMessageMention
  , lemContentProvider :: Maybe LineEventMessageContentProvider
  , lemImageSet        :: Maybe LineEventMessageImageSet
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessage where
    parseJSON (Object v) = LineEventMessage
        <$> v .: "id"
        <*> v .: "type"
        <*> v .:? "text"
        <*> v .:? "emojis"
        <*> v .:? "mention"
        <*> v .:? "contentProvider"
        <*> v .:? "imageSet"
    parseJSON invalid = prependFailure "parsing LineEventMessage failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessage where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 3

