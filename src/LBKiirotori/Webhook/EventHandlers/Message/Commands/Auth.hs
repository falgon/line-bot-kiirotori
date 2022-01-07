{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell, TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.Message.Commands.Auth (
    authCmd
) where

import           Control.Applicative                              (Alternative (..))
import           Control.Arrow                                    ((&&&), (|||))
import           Control.Exception.Safe                           (throwString)
import           Control.Monad                                    (unless)
import           Control.Monad                                    (join, liftM2)
import           Control.Monad.Extra                              (ifM)
import           Control.Monad.IO.Class                           (MonadIO (..))
import           Control.Monad.Logger                             (logError,
                                                                   logInfo,
                                                                   logWarn)
import           Control.Monad.Trans                              (lift)
import           Control.Monad.Trans.State                        (gets)
import           Data.Functor                                     ((<&>))
import qualified Data.Text                                        as T
import           Data.Time.LocalTime                              (LocalTime)
import qualified Data.Vector                                      as V
import           Data.Void
import           Database.MySQL.Base                              (MySQLValue (..),
                                                                   OK (..))
import qualified System.IO.Streams                                as S
import qualified Text.Megaparsec                                  as M
import qualified Text.Megaparsec.Char                             as MC

import           LBKiirotori.AccessToken                          (getAccessToken)
import           LBKiirotori.API.Count.Room
import           LBKiirotori.API.Profile.FriendUser
import           LBKiirotori.API.PushMessage
import           LBKiirotori.API.Summary.Group
import           LBKiirotori.Config                               (LBKiirotoriAppConfig (..),
                                                                   LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                   (MessageBody (..),
                                                                   textMessage)
import           LBKiirotori.Database.Redis                       (getPinCode)
import           LBKiirotori.Internal.Utils                       (fromMaybeM, getCurrentLocalTime,
                                                                   hoistMaybe,
                                                                   tshow)
import           LBKiirotori.Webhook.EventHandlers.Message.Event  (MessageEvent, MessageEventData (..))
import           LBKiirotori.Webhook.EventHandlers.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.Message.Utils  (replyOneText,
                                                                   srcId)
import           LBKiirotori.Webhook.EventObject.Core             (LineEventObject (..),
                                                                   LineEventType (..))
import           LBKiirotori.Webhook.EventObject.EventMessage     (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource      (LineEventSource (..),
                                                                   LineEventSourceType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler   (executeSQL,
                                                                   runSQL)

srcTypeVal :: Integral i => MessageEvent i
srcTypeVal = lift $ gets $
    fromIntegral
  . fromEnum
  . lineEventSrcType
  . lineEventSource
  . medLEO

checkAuthed :: MessageEvent (Maybe (LocalTime, T.Text))
checkAuthed = do
    qres <- sequence [
        MySQLText <$> srcId
      , MySQLInt8U <$> srcTypeVal
      ]
        >>= lift . lift . runSQL q
        >>= liftIO . S.toList . snd
        <&> V.concat
    pure $ if V.length qres /= 2 then Nothing else case (qres V.! 0, qres V.! 1) of
        (MySQLDateTime lt, MySQLText name) -> Just (lt, name)
        _                                  -> Nothing
    where
        q = "select created_at, name from authorized where id = ? and type = ?"

getDisplayName :: MessageEvent T.Text
getDisplayName = lift (gets $ lineEventSrcType . lineEventSource . medLEO) >>= \case
    LineEventSourceTypeUser     -> pfuDisplayName <$> pass profileFriendUser
    LineEventSourceTypeGroup    -> gsGroupName <$> pass groupSummary
    LineEventSourceTypeRoom     -> (<> "people room") . tshow . cmCount <$> pass countRoom
    where
        pass f = join
            $ liftM2 ((.) lift . f) (lift $ lift getAccessToken) (T.unpack <$> srcId)

authSuccess :: MessageEvent ()
authSuccess = do
    (r, c) <- sequence [
        MySQLText <$> srcId
      , MySQLInt8U <$> srcTypeVal
      , MySQLDateTime <$> getCurrentLocalTime
      , MySQLText <$> getDisplayName
      ]
      >>= lift . lift . executeSQL q
      <&> ((== 1) . okAffectedRows &&& (== 0) . okWarningCnt)
    if not r then lift $ lift $ throwString "expected only 1 row"
    else unless c $ lift $ lift $ $(logWarn) "There was a warning on insert into `authorized` table!"
    where
        q = "insert into authorized (id, type, created_at, name) values (?, ?, ?, ?)"

-- | the auth command, authenticate the code and register it in db if successful
-- <auth> ::= "auth" <space> (<space>*) <string>
authCmd :: MessageEvent ()
authCmd = (MC.string' "auth" *> lexeme MC.space1 *> checkAuthed) >>= \case
    Just (lt, name) -> lift (lift $ $(logInfo) "requested pincode command, but it is already authorized, send reply message")
        *> replyOneText (mconcat [ name, " は ", tshow lt, " に認証済みピ" ])
    Nothing -> replyOneText "認証中..."
        *> ifM ((==) <$> M.getInput <*> lift (lift getPinCode))
            (authSuccess *> replyOneText "認証成功ピ!")
            (replyOneText "認証コードが違うピ😥認証に失敗ピ😞")
