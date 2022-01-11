{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.Data.MessageObject (
    Sender (..)
  , textMessage
  , stickerMessage
  , MessageBody (..)
  , Messages
) where

import           Data.Aeson
import           Data.Char    (toLower)
import qualified Data.Text    as T
import           Data.Word    (Word32)
import           GHC.Generics

data Sender = Sender {
    senderName    :: T.Text
  , senderIconUrl :: T.Text
  } deriving (Eq, Show)

instance ToJSON Sender where
    toJSON (Sender n iu) = object [
        "name" .= n
      , "iconUrl" .= iu
      ]

data Emoji = Emoji {
    emojiIndex     :: Word32
  , emojiProductId :: T.Text
  , emojiId        :: T.Text
  } deriving (Eq, Show)

instance ToJSON Emoji where
    toJSON (Emoji idx pid sid) = object [
        "index" .= idx
      , "productId" .= pid
      , "emojiId" .= sid
      ]

data TextMessage = TextMessage {
    messageType   :: T.Text
  , messageText   :: T.Text
  , messageSender :: Maybe Sender
  , messageEmojis :: Maybe Emoji
  } deriving (Eq, Show, Generic)

textMessage :: T.Text
    -> Maybe Sender
    -> Maybe Emoji
    -> TextMessage
textMessage = TextMessage "text"

instance ToJSON TextMessage where
    toJSON = genericToJSON $ defaultOptions {
        omitNothingFields = True
      , fieldLabelModifier = map toLower . drop 7
      }

data StickerMessage = StickerMessage {
    stickerType      :: T.Text
  , stickerPackageId :: T.Text
  , stickerId        :: T.Text
  } deriving (Eq, Show)

stickerMessage :: T.Text -> T.Text -> StickerMessage
stickerMessage = StickerMessage "sticker"

instance ToJSON StickerMessage where
    toJSON (StickerMessage ty pid sid) = object [
        "type" .= ty
      , "packageId" .= pid
      , "stickerId" .= sid
      ]

data FlexMessageContentsBodyContent = FlexMessageContentsBodyContent {
    fmcbcType :: T.Text
  , fmcbcText :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON FlexMessageContentsBodyContent where
    toJSON = genericToJSON $ defaultOptions {
        fieldLabelModifier = map toLower . drop 5
      }

type FlexMessageContentsBodyContents = [FlexMessageContentsBodyContent]

data FlexMessageContentsBody = FlexMessageContentsBody {
    fmcbType     :: T.Text
  , fmcbLayout   :: T.Text
  , fmcbContents :: FlexMessageContentsBodyContents
  } deriving (Eq, Show, Generic)

instance ToJSON FlexMessageContentsBody where
    toJSON = genericToJSON $ defaultOptions {
        fieldLabelModifier = map toLower . drop 4
      }

data FlexMessageContents = FlexMessageContents {
    fmcType :: T.Text
  , fmcBody :: FlexMessageContentsBody
  } deriving (Eq, Show, Generic)

instance ToJSON FlexMessageContents where
    toJSON = genericToJSON $ defaultOptions {
        fieldLabelModifier = map toLower . drop 3
      }

data FlexMessage = FlexMessage {
    flexMessageType     :: T.Text
  , flexMessageAltText  :: T.Text
  , flexMessageContents :: FlexMessageContents
  } deriving (Eq, Show)

flexMessage :: T.Text -> FlexMessageContents -> FlexMessage
flexMessage = FlexMessage "flex"

instance ToJSON FlexMessage where
    toJSON (FlexMessage ty alt cts) = object [
        "type" .= ty
      , "altText" .= alt
      , "contents" .= cts
      ]

data MessageBody = MBText TextMessage
    | MBSticker StickerMessage
    | MBFlex FlexMessage
    deriving (Eq, Show)

instance ToJSON MessageBody where
    toJSON (MBText m)    = toJSON m
    toJSON (MBSticker m) = toJSON m
    toJSON (MBFlex m)    = toJSON m

type Messages = [MessageBody]
