{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell, TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.Message (
    messageEvent
) where

import           Control.Applicative                            (Alternative (..))
import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (throwString)
import           Control.Monad                                  (void)
import           Control.Monad.Extra                            (ifM)
import           Control.Monad.IO.Class                         (MonadIO (..))
import           Control.Monad.Logger                           (logError,
                                                                 logInfo)
import           Control.Monad.Reader                           (ReaderT (..),
                                                                 asks)
import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Maybe                      (MaybeT (..))
import           Data.Foldable                                  (asum)
import           Data.Functor                                   ((<&>))
import qualified Data.Text                                      as T
import qualified Data.Text.Encoding                             as T
import           Data.Time.LocalTime                            (LocalTime)
import qualified Data.Vector                                    as V
import           Data.Void
import           Database.MySQL.Base                            (MySQLValue (..))
import qualified System.IO.Streams                              as S
import qualified Text.Megaparsec                                as M
import qualified Text.Megaparsec.Char                           as MC
import qualified Text.Megaparsec.Char.Lexer                     as MCL

import           LBKiirotori.AccessToken                        (getAccessToken)
import           LBKiirotori.API.ReplyMessage
import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                 (MessageBody (..),
                                                                 textMessage)
import           LBKiirotori.Database.Redis                     (getPinCode)
import           LBKiirotori.Internal.Utils                     (hoistMaybe,
                                                                 tshow)
import           LBKiirotori.Webhook.EventObject.Core           (LineEventObject (..),
                                                                 LineEventType (..))
import           LBKiirotori.Webhook.EventObject.EventMessage   (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource    (LineEventSource (..),
                                                                 LineEventSourceType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler

spaceConsumer :: Ord e
    => M.ParsecT e T.Text m ()
spaceConsumer = MCL.space MC.space1 empty empty

lexeme :: Ord e
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m a
lexeme = MCL.lexeme spaceConsumer

-- <mention me> ::= (<space>*) "@kiirotori" <space>
mentionMeP :: Ord e
    => M.ParsecT e T.Text (MaybeT LineBotHandler) ()
mentionMeP = MC.space
    *> (lift (lift askLineChanName) >>= void . MC.string . (T.singleton '@' <>))
    <* MC.space1

-- <replied message> ::= <mention me> <cmd>
repliedMeParser :: M.ParsecT Void T.Text (MaybeT LineBotHandler) (Maybe T.Text)
repliedMeParser = M.option Nothing $ M.try (mentionMeP *> M.getInput <&> Just)

repliedMe :: LineEventMessage
    -> LineBotHandler (Maybe T.Text)
repliedMe mobj = runMaybeT $
    hoistMaybe (lemText mobj)
        >>= M.runParserT repliedMeParser mempty
        >>= (lift . throwString . M.errorBundlePretty ||| hoistMaybe)

data MessageEventData = MessageEventData {
    medTk  :: T.Text
  , medLEO :: LineEventObject
  }

type MessageEvent a = M.ParsecT Void T.Text (ReaderT MessageEventData LineBotHandler) a

-- | the echo command, echo the sent message as it is
-- <echo> ::= "echo" <space> <string>
echoCmd :: MessageEvent ()
echoCmd = do
    void (MC.string' "echo" *> MC.space1)
    txtBody <- M.getInput
    tk <- lift $ asks medTk
    lift $ lift $ do
        $(logInfo) "requested echo command, send reply message"
        ca <- getAccessToken
        replyMessage ca $ ReplyMessage {
            replyMessageReplyToken = tk
          , replyMessageMessages = [
                MBText $ textMessage txtBody Nothing Nothing
            ]
          , replyMessageNotificationDisabled = Nothing
          }

authed :: T.Text -> MessageEvent (Maybe (LocalTime, T.Text, T.Text))
authed tryCode = do
    src <- lift $ asks $ lineEventSource . medLEO
    case asum [ lineEventSrcUserId src, lineEventSrcGroupId src, lineEventSrcRoomId src ] of
        Nothing -> lift $ lift $
            throwString "need to be able to get the id of one of user, group, room"
        Just aId -> lift $ lift $ do
            qres <- runSQL q [
                MySQLText aId
              , MySQLInt8U $ fromIntegral $ fromEnum $ lineEventSrcType src
              ]
                >>= liftIO . S.toList . snd
                <&> V.concat
            if V.length qres /= 3 then pure Nothing else pure Nothing
    where
        q = "select created_at, group_name, authed_user_name from authorized where id = ? and type = ?"

-- | the pincode command, authenticate the code and register it in db if successful
-- <pincode> ::= "pincode" <space> <string>
pinCodeCmd :: MessageEvent ()
pinCodeCmd = do
    tryCode <- MC.string' "pincode" *> MC.space1 *> M.getInput
    authedInfo <- authed tryCode
    src <- lift $ asks $ lineEventSource . medLEO
    tk <- lift $ asks medTk
    pure ()

helpCmd :: MessageEvent ()
helpCmd = do
    void (lexeme (MC.string' "help") *> M.eof)
    tk <- lift $ asks medTk
    lift $ lift $ do
        $(logInfo) "requested help command, send reply message"
        ca <- getAccessToken
        replyMessage ca $ ReplyMessage {
            replyMessageReplyToken = tk
          , replyMessageMessages = [
                MBText $ textMessage "help!" Nothing Nothing
            ]
          , replyMessageNotificationDisabled = Nothing
          }

-- <cmd> ::= <echo>
--     | <pincode>
--     | <help>
cmd :: MessageEvent ()
cmd = M.choice [
    M.try echoCmd
  -- , M.try pinCodeCmd
  , M.try helpCmd
  , pure ()
  ]

messageEvent :: LineEventObject
    -> LineBotHandler ()
messageEvent e
    | lineEventType e == LineEventTypeMessage =
        case (,) <$> lineEventReplyToken e <*> lineEventMessage e of
            Nothing -> $(logError) "expected reply token and message object"
            Just (tk, mobj) -> repliedMe mobj >>= \case
                Nothing -> $(logInfo) "the message is not replied me"
                Just txtBody ->
                    runReaderT (M.runParserT cmd mempty txtBody) (MessageEventData tk e) >>= \case
                        Left err -> $(logError) $ T.pack $ M.errorBundlePretty err
                        Right () -> pure ()
    | otherwise = $(logError) "expected message event"
        >> $(logError) (tshow e)

