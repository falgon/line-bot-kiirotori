{-# LANGUAGE LambdaCase #-}
module LBKiirotori.Webhook.EventHandlers.Message.Utils (
    anyId
  , replyOneText
) where

import           Control.Exception.Safe                          (throwString)
import           Control.Monad                                   (join, liftM2)
import           Control.Monad.IO.Class                          (MonadIO (..))
import           Control.Monad.Trans                             (lift)
import           Control.Monad.Trans.State                       (gets, modify)
import qualified Data.Text                                       as T

import           LBKiirotori.AccessToken                         (getAccessToken)
import           LBKiirotori.API.PushMessage
import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Data.MessageObject                  (MessageBody (..),
                                                                  textMessage)
import           LBKiirotori.Internal.Utils                      (fromMaybeM)
import           LBKiirotori.Webhook.EventHandlers.Message.Event (MessageEvent, MessageEventData (..))
import           LBKiirotori.Webhook.EventObject.Core            (LineEventObject (..))
import           LBKiirotori.Webhook.EventObject.EventSource     (LineEventSource (..),
                                                                  LineEventSourceType (..))

anyId :: MessageEvent T.Text
anyId = do
    src <- lift $ gets $ lineEventSource . medLEO
    case lineEventSrcType src of
        LineEventSourceTypeUser -> fromMaybeM
            (lift $ lift $ throwString "need to be able to get the id of one of user")
            $ lineEventSrcUserId src
        LineEventSourceTypeGroup -> fromMaybeM
            (lift $ lift $ throwString "need to be able to get the id of one of group")
            $ lineEventSrcGroupId src
        LineEventSourceTypeRoom -> fromMaybeM
            (lift $ lift $ throwString "need to be able to get the id of one of room")
            $ lineEventSrcRoomId src

replyTextData :: [T.Text] -> MessageEvent (Maybe ReplyMessage)
replyTextData txts = lift (gets medTk) >>= \case
    Just tk -> (pure $ Just $ ReplyMessage {
        replyMessageReplyToken = tk
      , replyMessageMessages = map (\x -> MBText $ textMessage x Nothing Nothing) txts
      , replyMessageNotificationDisabled = Nothing
      }) <* lift (modify (\d -> d { medTk = Nothing }))
    Nothing -> pure Nothing

replyText :: [T.Text] -> MessageEvent ()
replyText txts = replyTextData txts >>= \case
    Just rot -> (lift $ lift getAccessToken) >>= lift . flip replyMessage rot
    Nothing -> do
        ca <- lift $ lift getAccessToken
        aId <- anyId
        liftIO $ pushMessage ca $ PushMessage {
            pmTo = aId
          , pmMessages = map (\x -> MBText $ textMessage x Nothing Nothing) txts
          }

replyOneText :: T.Text -> MessageEvent ()
replyOneText = replyText . (:[])
