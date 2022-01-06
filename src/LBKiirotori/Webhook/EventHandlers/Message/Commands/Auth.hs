{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell, TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.Message.Commands.Auth (
    authCmd
) where

import           Control.Applicative                              (Alternative (..))
import           Control.Arrow                                    ((|||))
import           Control.Exception.Safe                           (throwString)
import           Control.Lens.Lens                                ((??))
import           Control.Monad                                    (void)
import           Control.Monad.Extra                              (ifM)
import           Control.Monad.IO.Class                           (MonadIO (..))
import           Control.Monad.Logger                             (logError,
                                                                   logInfo)
import           Control.Monad.Reader                             (ReaderT (..),
                                                                   asks)
import           Control.Monad.Trans                              (lift)
import           Control.Monad.Trans.Maybe                        (MaybeT (..))
import           Data.Foldable                                    (asum)
import           Data.Functor                                     ((<&>))
import qualified Data.Text                                        as T
import qualified Data.Text.Encoding                               as T
import           Data.Time.LocalTime                              (LocalTime)
import qualified Data.Vector                                      as V
import           Data.Void
import           Database.MySQL.Base                              (MySQLValue (..))
import qualified System.IO.Streams                                as S
import qualified Text.Megaparsec                                  as M
import qualified Text.Megaparsec.Char                             as MC
import qualified Text.Megaparsec.Char.Lexer                       as MCL

import           LBKiirotori.Config                               (LBKiirotoriAppConfig (..),
                                                                   LBKiirotoriConfig (..))
import           LBKiirotori.Database.Redis                       (getPinCode)
import           LBKiirotori.Internal.Utils                       (tshow)
import           LBKiirotori.Webhook.EventHandlers.Message.Event  (MessageEvent, MessageEventData (..))
import           LBKiirotori.Webhook.EventHandlers.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.Message.Utils  (replyOneText)
import           LBKiirotori.Webhook.EventObject.Core             (LineEventObject (..),
                                                                   LineEventType (..))
import           LBKiirotori.Webhook.EventObject.EventMessage     (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource      (LineEventSource (..),
                                                                   LineEventSourceType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler   (runSQL)

authedQuery :: T.Text
    -> LineEventSource
    -> MessageEvent (Maybe (LocalTime, T.Text))
authedQuery aId src = lift $ lift $ do
    qres <- runSQL q [
        MySQLText aId
      , MySQLInt8U $ fromIntegral $ fromEnum $ lineEventSrcType src
      ]
        >>= liftIO . S.toList . snd
        <&> V.concat
    if V.length qres /= 2 then pure Nothing else case (qres V.! 0, qres V.! 1) of
        (MySQLDateTime lt, MySQLText name) -> pure $ Just (lt, name)
        _                                  -> pure $ Nothing
    where
        q = "select created_at, name from authorized where id = ? and type = ?"

authed :: T.Text -> MessageEvent (Maybe (LocalTime, T.Text))
authed tryCode = do
    src <- lift $ asks $ lineEventSource . medLEO
    maybe (lift $ lift $ throwString "need to be able to get the id of one of user, group, room")
        (flip authedQuery src)
        $ asum $ [ lineEventSrcUserId, lineEventSrcGroupId, lineEventSrcRoomId ] ?? src

-- | the auth command, authenticate the code and register it in db if successful
-- <auth> ::= "auth" <space> (<space>*) <string>
authCmd :: MessageEvent ()
authCmd = do
    tryCode <- MC.string' "auth" *> lexeme MC.space1 *> M.getInput
    tk <- lift $ asks medTk
    authed tryCode >>= \case
        Just (lt, name) -> do
            lift $ lift $
                $(logInfo)
                    "requested pincode command, but it is already authorized, send reply message"
            replyOneText (authorizedMsg lt name)
        Nothing -> pure ()
    where
        authorizedMsg lt name = mconcat [ name, " は ", tshow lt, " に認証済みピ" ]


