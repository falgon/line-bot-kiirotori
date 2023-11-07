{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TemplateHaskell,
             TupleSections #-}
module LBKiirotori.Webhook.EventHandlers.LINE.Message.Commands.Auth (
    checkAuthed
  , authCmd
) where

import           Control.Applicative                                   (Alternative (..))
import           Control.Arrow                                         ((&&&),
                                                                        (|||))
import           Control.Exception.Safe                                (throwString)
import           Control.Monad                                         (join,
                                                                        liftM2,
                                                                        unless,
                                                                        (>=>))
import           Control.Monad.Extra                                   (ifM)
import           Control.Monad.IO.Class                                (MonadIO (..))
import           Control.Monad.Logger                                  (logError,
                                                                        logInfo,
                                                                        logWarn)
import           Control.Monad.Trans                                   (lift)
import           Control.Monad.Trans.Maybe                             (MaybeT (..))
import           Control.Monad.Trans.Reader                            (asks)
import           Control.Monad.Trans.State                             (gets)
import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.UTF8                                  as BS
import           Data.Functor                                          ((<&>))
import           Data.Proxy
import           Data.String                                           (IsString (..))
import qualified Data.Text                                             as T
import qualified Data.Text.Encoding                                    as T
import           Data.Time.Clock.POSIX                                 (utcTimeToPOSIXSeconds)
import           Data.Time.LocalTime                                   (LocalTime)
import           Data.Tuple.Extra                                      (firstM,
                                                                        second)
import qualified Data.Vector                                           as V
import qualified Data.Vector.Fixed                                     as VF
import qualified Data.Vector.Fixed.Boxed                               as VF
import           Data.Void
import           Database.MySQL.Base                                   (MySQLValue (..),
                                                                        OK (..))
import           Database.Redis                                        (Status (..))
import qualified System.IO.Streams                                     as S
import qualified Text.Megaparsec                                       as M
import qualified Text.Megaparsec.Char                                  as MC
import           Text.Printf                                           (printf)
import           Text.Read                                             (readMaybe)

import           LBKiirotori.AccessToken                               (getAccessToken)
import           LBKiirotori.API.Count.Room
import           LBKiirotori.API.Profile.FriendUser
import           LBKiirotori.API.PushMessage
import           LBKiirotori.API.Summary.Group
import           LBKiirotori.Config                                    (LBKiirotoriAppConfig (..),
                                                                        LBKiirotoriConfig (..))
import           LBKiirotori.Data.MessageObject                        (MessageBody (..),
                                                                        textMessage)
import           LBKiirotori.Database.Redis                            (getAuthCode,
                                                                        hmget',
                                                                        hmset')
import           LBKiirotori.Internal.Utils                            (doubleToUTCTime,
                                                                        getCurrentLocalTime,
                                                                        hoistMaybe,
                                                                        localTimeToCurrentUTCTimeZone,
                                                                        tshow,
                                                                        utcToCurrentLocalTimeZone)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Event  (MessageEvent,
                                                                        MessageEventData (..),
                                                                        getLineEventSrc)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Parser (lexeme)
import           LBKiirotori.Webhook.EventHandlers.LINE.Message.Utils  (replyOneText,
                                                                        srcId)
import           LBKiirotori.Webhook.EventObject.Core                  (LineEventObject (..),
                                                                        LineEventType (..))
import           LBKiirotori.Webhook.EventObject.EventMessage          (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.EventSource           (LineEventSource (..),
                                                                        LineEventSourceType (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler        (LineBotHandlerConfig (..),
                                                                        executeSQL,
                                                                        runRedis,
                                                                        runSQL)

srcTypeVal :: Integral i => MessageEvent i
srcTypeVal = getLineEventSrc
    <&> fromIntegral . fromEnum . lineEventSrcType

kVSEncodedKey :: MessageEvent BS.ByteString
kVSEncodedKey = (\v i -> fromString (show v) <> ":" <> T.encodeUtf8 i)
    <$> srcTypeVal
    <*> srcId

writeKVSAuthInfo :: LocalTime -> T.Text -> MessageEvent ()
writeKVSAuthInfo lt name = do
    key <- kVSEncodedKey
    lift $ lift $ runRedis
        (localTimeToCurrentUTCTimeZone lt
            >>= hmset' key
                . (:[("name", T.encodeUtf8 name)])
                . ("created_at",)
                . fromString
                . show
                . realToFrac
                . utcTimeToPOSIXSeconds)
        >>= \case
            Ok -> pure ()
            _  -> throwString "unexpected to return status, expected Ok"

data DataSrc = FromRDB LocalTime T.Text
    | FromKVS LocalTime T.Text

srcLocalTime :: DataSrc -> LocalTime
srcLocalTime (FromRDB lt _) = lt
srcLocalTime (FromKVS lt _) = lt

srcName :: DataSrc -> T.Text
srcName (FromRDB _ name) = name
srcName (FromKVS _ name) = name

checkAuthedKVS :: MessageEvent (Maybe DataSrc)
checkAuthedKVS = runMaybeT $ do
    key <- lift kVSEncodedKey
    lift (lift $ lift $ runRedis $ hmget' key fields)
        >>= hoistMaybe
        >>= firstM (hoistMaybe . readMaybe . BS.toString >=> utcToCurrentLocalTimeZone . doubleToUTCTime) . VF.convert
        <&> uncurry FromKVS . second T.decodeUtf8
    where
        fields :: VF.Vec2 BS.ByteString
        fields = VF.mk2 "created_at" "name"

checkAuthedRDB :: MessageEvent (Maybe DataSrc)
checkAuthedRDB = do
    qres <- sequence [
        MySQLText <$> srcId
      , MySQLInt8U <$> srcTypeVal
      ]
        >>= lift . lift . runSQL q
        >>= liftIO . S.toList . snd
        <&> V.concat
    pure $ if V.length qres /= 2 then Nothing else case (qres V.! 0, qres V.! 1) of
        (MySQLDateTime lt, MySQLText name) -> Just $ FromRDB lt name
        _                                  -> Nothing
    where
        q = "select created_at, name from authorized where id = ? and type = ?"

checkAuthed :: MessageEvent (Maybe DataSrc)
checkAuthed = runMaybeT $
    ((<|>) <$> lift checkAuthedKVS <*> lift checkAuthedRDB)
        >>= hoistMaybe
        >>= \case
            r@(FromRDB lt name) -> r <$ lift (writeKVSAuthInfo lt name)
            x                   -> pure x

getDisplayName :: MessageEvent T.Text
getDisplayName = getLineEventSrc <&> lineEventSrcType >>= \case
    LineEventSourceTypeUser     -> pfuDisplayName <$> pass profileFriendUser
    LineEventSourceTypeGroup    -> gsGroupName <$> pass groupSummary
    LineEventSourceTypeRoom     -> (<> "people room") . tshow . cmCount <$> pass countRoom
    where
        pass f = join
            $ liftM2 ((.) lift . f) (lift $ lift getAccessToken) (T.unpack <$> srcId)

authSuccess :: MessageEvent ()
authSuccess = do
    lt <- getCurrentLocalTime
    (r, c) <- sequence [
        MySQLText <$> srcId
      , MySQLInt8U <$> srcTypeVal
      , pure $ MySQLDateTime lt
      , MySQLText <$> getDisplayName
      ]
      >>= lift . lift . executeSQL q
      <&> ((== 1) . okAffectedRows &&& (== 0) . okWarningCnt)
    if not r then lift $ lift $ throwString "expected only 1 row"
    else unless c $ do
        lift $ lift $ $(logWarn) "There was a warning on insert into `authorized` table!"
        getDisplayName >>= writeKVSAuthInfo lt
    where
        q = "insert into authorized (id, type, created_at, name) values (?, ?, ?, ?)"

authed :: DataSrc -> MessageEvent ()
authed authedInfo = do
    lift $ lift $ $(logInfo)
        "requested pincode command, but it is already authorized, send reply message"
    p <- lift (lift $ asks $ cfgAppAlreadyAuth . cfgApp . lbhCfg)
        <&> printf . fromString . T.unpack
    replyOneText $ T.pack $ p (srcName authedInfo) $ tshow $ srcLocalTime authedInfo

notAuthed :: MessageEvent ()
notAuthed = lift (lift $ asks $ cfgAppDuringAuth . cfgApp . lbhCfg)
    >>= replyOneText
    >> ifM ((==) <$> M.getInput <*> lift (lift getAuthCode))
        (authSuccess *> lift (lift $ asks $ cfgAppSuccessAuth . cfgApp . lbhCfg) >>= replyOneText)
        (lift (lift $ asks $ cfgAppFailedAuth . cfgApp . lbhCfg) >>= replyOneText)

-- | the auth command, authenticate the code and register it in db if successful
-- <auth> ::= "auth" <space> (<space>*) <string>
authCmd :: MessageEvent ()
authCmd = (MC.string' "auth" *> lexeme MC.space1 *> checkAuthed)
    >>= maybe notAuthed authed
