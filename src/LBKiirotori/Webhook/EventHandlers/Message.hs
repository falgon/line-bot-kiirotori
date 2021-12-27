{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell, TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.Message (
    messageEvent
) where

import           Control.Applicative                            (Alternative (..))
import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (throwString)
import           Control.Monad                                  (void)
import           Control.Monad.Extra                            (ifM)
import           Control.Monad.Logger                           (logError,
                                                                 logInfo)
import           Control.Monad.Reader                           (asks)
import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Maybe                      (MaybeT (..))
import           Data.Functor                                   ((<&>))
import qualified Data.Text                                      as T
import qualified Data.Text.Encoding                             as T
import           Data.Void
import qualified Text.Megaparsec                                as M
import qualified Text.Megaparsec.Char                           as MC
import qualified Text.Megaparsec.Char.Lexer                     as MCL

import           LBKiirotori.AccessToken                        (getAccessToken)
import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                 (MessageBody (..),
                                                                 textMessage)
import           LBKiirotori.Internal.Utils                     (hoistMaybe,
                                                                 tshow)
import           LBKiirotori.Webhook.EventObject.Core           (LineEventObject (..),
                                                                 LineEventType (..))
import           LBKiirotori.Webhook.EventObject.EventMessage   (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource    (LineEventSource (..),
                                                                 LineEventSourceType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler

-- <mention me> ::= (<space>*) "@kiirotori" <space>
mentionMeP :: Ord e
    => M.ParsecT e T.Text (MaybeT LineBotHandler) ()
mentionMeP = MC.space
    *> (lift (lift askLineChanName) >>= void . MC.string . (T.singleton '@' <>))
    <* MC.space1

-- <replied message> ::= <mention me> <string>
repliedMeParser :: M.ParsecT Void T.Text (MaybeT LineBotHandler) (Maybe T.Text)
repliedMeParser = M.option Nothing $ M.try (mentionMeP *> M.getInput <&> Just)

isRepliedMe :: LineEventSource
    -> LineEventMessage
    -> LineBotHandler (Maybe T.Text)
isRepliedMe src mobj = runMaybeT $
    hoistMaybe (lemText mobj)
        >>= M.runParserT repliedMeParser mempty
        >>= (lift . throwString . M.errorBundlePretty ||| hoistMaybe)

messageEvent :: LineEventObject
    -> LineBotHandler ()
messageEvent e
    | lineEventType e == LineEventTypeMessage =
        case (,) <$> lineEventReplyToken e <*> lineEventMessage e of
            Nothing -> $(logError) "expected reply token and message object"
            Just (tk, mobj) -> isRepliedMe (lineEventSource e) mobj >>= \case
                Nothing -> $(logInfo) "the message is not replied me"
                Just txtBody -> do
                    $(logInfo) "send reply message"
                    caToken <- getAccessToken
                    replyMessage caToken $ ReplyMessage {
                        replyMessageReplyToken = tk
                      , replyMessageMessages = [
                            MBText $ textMessage txtBody Nothing Nothing
                        ]
                      , replyMessageNotificationDisabled = Nothing
                      }
    | otherwise = $(logError) "expected message event"
        >> $(logError) (tshow e)

