{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.Webhook.EventObject.EventMessage (
    LineEventMessageType (..)
  , LineEventMessageMentionee (..)
  , LineEventMessageMention (..)
  , LineEventMessageEmoji (..)
  , LineEventMessageStickerResourceType (..)
  , LineEventMessageUnsend (..)
  , LineEventMessageJoined (..)
  , LineEventMessagePostbackParams (..)
  , LineEventMessagePostback (..)
  , LineEventMessage (..)
) where

import           Data.Aeson                                  (FromJSON (..),
                                                              ToJSON (..),
                                                              defaultOptions,
                                                              fieldLabelModifier,
                                                              genericToJSON,
                                                              object, (.:),
                                                              (.:?), (.=))
import           Data.Aeson.Types                            (Value (..),
                                                              prependFailure,
                                                              typeMismatch)
import           Data.Scientific
import qualified Data.Text                                   as T
import           GHC.Generics

import           LBKiirotori.Internal.Utils                  (stripFirstToLowerLabeledOption)
import           LBKiirotori.Webhook.EventObject.EventSource (LineEventSource)

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#webhook-event-objects
--
-- Beacon event, account linkage, device linkage are not planned to be used at this time,
-- so they are not supported.
-- https://developers.line.biz/ja/reference/messaging-api/#beacon-event

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
    parseJSON invalid = prependFailure "parsing LineEventMessageImageSet failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageImageSet where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

data LineEventMessageStickerResourceType = LineEventMessageStickerResourceTypeSTATIC
    | LineEventMessageStickerResourceTypeANIMATION
    | LineEventMessageStickerResourceTypeSOUND
    | LineEventMessageStickerResourceTypeANIMATION_SOUND
    | LineEventMessageStickerResourceTypePOPUP
    | LineEventMessageStickerResourceTypePOPUP_SOUND
    | LineEventMessageStickerResourceTypeNAME_TEXT
    | LineEventMessageStickerResourceTypePER_STICKER_TEXT
    | LineEventMessageStickerResourceTypeOther
    deriving (Eq, Show)

instance FromJSON LineEventMessageStickerResourceType where
    parseJSON (String "STATIC") = pure LineEventMessageStickerResourceTypeSTATIC
    parseJSON (String "ANIMATION") = pure LineEventMessageStickerResourceTypeANIMATION
    parseJSON (String "SOUND") = pure LineEventMessageStickerResourceTypeSOUND
    parseJSON (String "ANIMATION_SOUND") = pure LineEventMessageStickerResourceTypeANIMATION_SOUND
    parseJSON (String "POPUP") = pure LineEventMessageStickerResourceTypePOPUP
    parseJSON (String "POPUP_SOUND") = pure LineEventMessageStickerResourceTypePOPUP_SOUND
    parseJSON (String "NAME_TEXT") = pure LineEventMessageStickerResourceTypeNAME_TEXT
    parseJSON (String "PER_STICKER_TEXT") = pure LineEventMessageStickerResourceTypePER_STICKER_TEXT
    parseJSON (String _) = pure LineEventMessageStickerResourceTypeOther
    parseJSON invalid = prependFailure "parsing LineEventMessageStickerResourceType failed, "
        $ typeMismatch "String" invalid

instance ToJSON LineEventMessageStickerResourceType where
    toJSON LineEventMessageStickerResourceTypeSTATIC = String "STATIC"
    toJSON LineEventMessageStickerResourceTypeANIMATION = String "ANIMATION"
    toJSON LineEventMessageStickerResourceTypeSOUND = String "SOUND"
    toJSON LineEventMessageStickerResourceTypeANIMATION_SOUND = String "ANIMATION_SOUND"
    toJSON LineEventMessageStickerResourceTypePOPUP = String "POPUP"
    toJSON LineEventMessageStickerResourceTypePOPUP_SOUND = String "POPUP_SOUND"
    toJSON LineEventMessageStickerResourceTypeNAME_TEXT = String "NAME_TEXT"
    toJSON LineEventMessageStickerResourceTypePER_STICKER_TEXT = String "PER_STICKER_TEXT"
    toJSON _ = Null

newtype LineEventMessageUnsend = LineEventMessageUnsend {
    lemuMessageId :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageUnsend where
    parseJSON (Object v) = LineEventMessageUnsend
        <$> v .: "messageId"
    parseJSON invalid = prependFailure "parsing LineEventMessageUnsend failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageUnsend where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

newtype LineEventMessageJoined = LineEventMessageJoined {
    lemjMembers :: [LineEventSource]
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageJoined where
    parseJSON (Object v) = LineEventMessageJoined
        <$> v .: "members"
    parseJSON invalid = prependFailure "parsing LineEventMessageJoined failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageJoined where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

newtype LineEventMessageLeft = LineEventMessageLeft {
    lemlMembers :: [LineEventSource]
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageLeft where
    parseJSON (Object v) = LineEventMessageLeft
        <$> v .: "members"
    parseJSON invalid = prependFailure "parsing LineEventMessageLeft failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageLeft where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

data LineEventMessagePostbackParams = LineEventMessagePostbackParams {
    lemppDate               :: Maybe T.Text
  , lemppTime               :: Maybe T.Text
  , lemppDatetime           :: Maybe T.Text
  , lemppNewRichMenuAliasId :: Maybe T.Text
  , lemppStatus             :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessagePostbackParams where
    parseJSON (Object v) = LineEventMessagePostbackParams
        <$> v .:? "date"
        <*> v .:? "time"
        <*> v .:? "datetime"
        <*> v .:? "newRichMenuAliasId"
        <*> v .:? "status"
    parseJSON invalid = prependFailure "parsing LineEventMessagePostbackParams failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessagePostbackParams where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 5

data LineEventMessagePostback = LineEventMessagePostback {
    lempData   :: T.Text
  , lempParams :: LineEventMessagePostbackParams
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessagePostback where
    parseJSON (Object v) = LineEventMessagePostback
        <$> v .: "data"
        <*> v .: "params"
    parseJSON invalid = prependFailure "parsing LineEventMessagePostback failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessagePostback where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 4

newtype LineEventMessageVideoPlayComplete = LineEventMessageVideoPlayComplete {
    lemvpcTrackingId :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LineEventMessageVideoPlayComplete where
    parseJSON (Object v) = LineEventMessageVideoPlayComplete
        <$> v .: "videoPlayComplete"
    parseJSON invalid = prependFailure "parsing LineEventMessageVideoPlayComplete failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessageVideoPlayComplete where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 6

data LineEventMessage = LineEventMessage {
    lemId                  :: T.Text
  , lemType                :: LineEventMessageType
  , lemText                :: Maybe T.Text
  , lemEmojis              :: Maybe LineEventMessageEmojis
  , lemMention             :: Maybe LineEventMessageMention
  , lemContentProvider     :: Maybe LineEventMessageContentProvider
  , lemImageSet            :: Maybe LineEventMessageImageSet
  , lemDuration            :: Maybe Scientific
  , lemFileName            :: Maybe T.Text
  , lemFileSize            :: Maybe Scientific
  , lemTitle               :: Maybe T.Text
  , lemAddress             :: Maybe T.Text
  , lemLatitude            :: Maybe Scientific
  , lemLongitude           :: Maybe Scientific
  , lemPackageId           :: Maybe T.Text
  , lemStickerId           :: Maybe T.Text
  , lemStickerResourceType :: Maybe LineEventMessageStickerResourceType
  , lemKeywords            :: Maybe [T.Text]
  , lemUnsend              :: Maybe LineEventMessageUnsend
  , lemJoined              :: Maybe LineEventMessageJoined
  , lemLeft                :: Maybe LineEventMessageLeft
  , lemPostback            :: Maybe LineEventMessagePostback
  , lemVideoPlayComplete   :: Maybe LineEventMessageVideoPlayComplete
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
        <*> v .:? "duration"
        <*> v .:? "fileName"
        <*> v .:? "fileSize"
        <*> v .:? "title"
        <*> v .:? "address"
        <*> v .:? "latitude"
        <*> v .:? "longitude"
        <*> v .:? "packageId"
        <*> v .:? "stickerId"
        <*> v .:? "stickerResourceType"
        <*> v .:? "keywords"
        <*> v .:? "unsend"
        <*> v .:? "joined"
        <*> v .:? "left"
        <*> v .:? "postback"
        <*> v .:? "videoPlayComplete"
    parseJSON invalid = prependFailure "parsing LineEventMessage failed, "
        $ typeMismatch "Object" invalid

instance ToJSON LineEventMessage where
    toJSON = genericToJSON $ stripFirstToLowerLabeledOption 3

