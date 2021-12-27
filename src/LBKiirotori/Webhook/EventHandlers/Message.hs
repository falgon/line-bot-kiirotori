{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell, TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.Message (
    messageEvent
) where

import           Control.Applicative                            (Alternative (..))
import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (throw)
import           Control.Monad                                  (void, (>=>))
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

    {-
messageEvent :: LineEventObject
    -> LineBotHandler ()
messageEvent e
    | lineEventType e == LineEventTypeMessage = case lineEventReplyToken e of
        Nothing -> $(logError) "expected reply token"
        Just tk -> do
            caToken <- getAccessToken
            -- messageMessage <- asks $ cfgAppWelcome . cfgApp . lbhCfg
            $(logInfo) "send reply message"
            replyMessage caToken $ ReplyMessage {
                replyMessageReplyToken = tk
              , replyMessageMessages = [
                    MBText $ textMessage messageMessage Nothing Nothing
                  ]
              , replyMessageNotificationDisabled = Nothing
              }
    | otherwise = $(logError) "expected message event"
        >> $(logError) (tshow e)
-}

spaceConsumer :: Ord e
    => M.ParsecT e T.Text m ()
spaceConsumer = MCL.space MC.space1 M.empty M.empty

lexeme :: Ord e
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m a
lexeme = MCL.lexeme spaceConsumer

mentionMeP :: Ord e
    => M.ParsecT e T.Text (MaybeT LineBotHandler) ()
mentionMeP = spaceConsumer
    *> (lift (lift askLineChanName) >>= void . lexeme . MC.string . (T.singleton '@' <>))

repliedMeParser :: M.ParsecT Void T.Text (MaybeT LineBotHandler) (Maybe T.Text)
repliedMeParser = M.option Nothing (mentionMeP *> M.getInput <&> Just)

isReplyMe :: LineEventSource
    -> LineEventMessage
    -> LineBotHandler (Maybe T.Text)
isReplyMe src mobj = runMaybeT $
    ifM ((||)
        <$> pure (lineEventSrcType src /= LineEventSourceTypeUser)
        <*> ((==) <$> hoistMaybe (lineEventSrcUserId src) <*> (T.decodeUtf8 <$> lift askLineUserId)))
        empty
        (hoistMaybe (lemText mobj)
            >>= M.runParserT repliedMeParser mempty
            >>= (lift . throw ||| hoistMaybe))

messageEvent :: LineEventObject
    -> LineBotHandler ()
messageEvent e
    | lineEventType e == LineEventTypeMessage =
        case (,) <$> lineEventReplyToken e <*> lineEventMessage e of
            Nothing -> $(logError) "expected reply token and message object"
            Just (tk, mobj) -> isReplyMe (lineEventSource e) (mobj) >>= \case
                Nothing -> pure ()
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

