{-# LANGUAGE LambdaCase #-}
module LBKiirotori.Webhook.EventHandlers.Message.Utils (
    srcId
  , replyOneText
  , replyText
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

srcId :: MessageEvent T.Text
srcId = do
    src <- lift $ gets $ lineEventSource . medLEO
    case lineEventSrcType src of
        LineEventSourceTypeUser  -> idRecord "user" lineEventSrcUserId src
        LineEventSourceTypeGroup -> idRecord "group" lineEventSrcGroupId src
        LineEventSourceTypeRoom  -> idRecord "room" lineEventSrcRoomId src
        where
            idRecord s f src = fromMaybeM
                (lift $ lift $ throwString $ mconcat [ "need to be able to get the id of ", s ])
                $ f src

replyTextData :: [T.Text] -> MessageEvent (Maybe ReplyMessage)
replyTextData txts = lift (gets medTk) >>= \case
    Just tk -> Just (replyData tk)
        <$ lift (modify (\d -> d { medTk = Nothing }))
    Nothing -> pure Nothing
    where
        replyData tk = ReplyMessage {
            replyMessageReplyToken = tk
          , replyMessageMessages = map (\x -> MBText $ textMessage x Nothing Nothing) txts
          , replyMessageNotificationDisabled = Nothing
          }

replyText :: [T.Text] -> MessageEvent ()
replyText txts = replyTextData txts >>= \case
    Just rot -> lift (lift getAccessToken)
        >>= lift . flip replyMessage rot
    Nothing -> join $ ((.) liftIO . pushMessage <$> lift (lift getAccessToken))
        <*> (pushMessageData <$> srcId)
    where
        pushMessageData aId = PushMessage {
            pmTo = aId
          , pmMessages = map (\x -> MBText $ textMessage x Nothing Nothing) txts
          }

replyOneText :: T.Text -> MessageEvent ()
replyOneText = replyText . (:[])
