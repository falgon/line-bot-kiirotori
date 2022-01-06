module LBKiirotori.Webhook.EventHandlers.Message.Utils (
    replyOneText
) where

import           Control.Monad                                   (join, liftM2)
import           Control.Monad.Trans                             (lift)
import           Control.Monad.Trans.Reader                      (asks)
import qualified Data.Text                                       as T

import           LBKiirotori.AccessToken                         (getAccessToken)
import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Data.MessageObject                  (MessageBody (..),
                                                                  textMessage)
import           LBKiirotori.Webhook.EventHandlers.Message.Event (MessageEvent, MessageEventData (..))

replyOneTextData :: T.Text -> MessageEvent ReplyMessage
replyOneTextData txtBody = do
    tk <- lift (asks medTk)
    pure $ ReplyMessage {
        replyMessageReplyToken = tk
      , replyMessageMessages = [
            MBText $ textMessage txtBody Nothing Nothing
          ]
      , replyMessageNotificationDisabled = Nothing
      }

replyOneText :: T.Text -> MessageEvent ()
replyOneText = join
    . liftM2 ((.) lift . replyMessage) (lift $ lift getAccessToken)
    . replyOneTextData
