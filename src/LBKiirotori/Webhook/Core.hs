{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module LBKiirotori.Webhook.Core (
    mainServer
) where

import           Control.Arrow                                  ((&&&), (|||))
import           Control.Exception.Safe                         (MonadThrow)
import           Control.Monad                                  (forM_, unless)
import           Control.Monad.Except                           (MonadError (..))
import           Control.Monad.Extra                            (ifM)
import           Control.Monad.IO.Class                         (MonadIO (..))
import           Control.Monad.Logger                           (LoggingT (..),
                                                                 logError,
                                                                 logInfo,
                                                                 runStdoutLoggingT)
import           Control.Monad.Parallel                         as MP
import           Control.Monad.Reader                           (ReaderT (..),
                                                                 asks)
import           Crypto.Hash.SHA256                             (hmac)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                                as B
import qualified Data.ByteString.Base64                         as Base64
import qualified Data.ByteString.Lazy                           as BL
import           Data.Functor                                   (($>))
import qualified Data.HashMap.Strict                            as HM
import qualified Data.List.NonEmpty                             as NE
import           Data.String                                    (IsString (..))
import qualified Data.Text                                      as T
import qualified Data.Text.Encoding                             as T
import qualified Data.Vector                                    as V
import qualified Network.HTTP.Media                             as M
import           Network.Wai.Handler.Warp                       (Port, Settings,
                                                                 defaultSettings,
                                                                 runSettings,
                                                                 setBeforeMainLoop,
                                                                 setPort)
import qualified Options.Applicative.Help.Pretty                as OA
import qualified Path                                           as P
import           Servant                                        (Header, JSON,
                                                                 MimeRender (..),
                                                                 MimeUnrender (..),
                                                                 PlainText,
                                                                 Post,
                                                                 Proxy (..),
                                                                 ReqBody,
                                                                 type (:>))
import           Servant.API.ContentTypes                       (Accept (..))
import           Servant.Server                                 (Application,
                                                                 Handler (..),
                                                                 Server,
                                                                 hoistServer,
                                                                 serve)
import           Servant.Server.Internal.ServerError

import           LBKiirotori.Config                             (LBKiirotoriAppConfig (..),
                                                                 LBKiirotoriConfig (..),
                                                                 readConfigWithLog)
import qualified LBKiirotori.Database.MySQL                     as MySQL
import qualified LBKiirotori.Database.Redis                     as Redis
import           LBKiirotori.Internal.Utils                     (tshow)
import           LBKiirotori.Webhook.EventHandlers
import           LBKiirotori.Webhook.EventObject
import           LBKiirotori.Webhook.EventObject.LineBotHandler

type LineSignature = T.Text

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#request-body
data LineWebhookRequestBody = LineWebhookRequestBody {
    lineWHRBDst    :: T.Text
  , lineWHRBEvents :: [LineEventObject]
  } deriving Show

instance FromJSON LineWebhookRequestBody where
    parseJSON (Object v) = LineWebhookRequestBody
        <$> v .: "destination"
        <*> v .: "events"

instance ToJSON LineWebhookRequestBody where
    toJSON v = Object $ HM.fromList [
        ("destination", String $ lineWHRBDst v)
      , ("events", toJSON $ lineWHRBEvents v)
      ]

-- Holds the raw string once because it requires hmac encoding
data WebhookJSON

instance Accept WebhookJSON where
    contentTypes _ =
        "application" M.// "json" M./: ("charset", "utf-8") NE.:|
        [ "application" M.// "json" ]

instance MimeRender WebhookJSON BL.ByteString where
    mimeRender _ = id

instance MimeRender WebhookJSON B.ByteString where
    mimeRender _ = BL.fromStrict

instance MimeUnrender WebhookJSON BL.ByteString where
    mimeUnrender _ = Right

instance MimeUnrender WebhookJSON B.ByteString where
    mimeUnrender _ = Right . BL.toStrict

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#request-headers
type API = "linebot"
    :> "webhook"
    :> Header "x-line-signature" LineSignature
    :> ReqBody '[WebhookJSON] B.ByteString
    :> Post '[PlainText] T.Text

eventHandler :: LineEventObject
    -> LineBotHandler ()
eventHandler e
    | lineEventType e == LineEventTypeJoin = joinEvent e
    | lineEventType e == LineEventTypeMessage = messageEvent e
    | otherwise = $(logInfo) (tshow e)

mainHandler' :: LineWebhookRequestBody
    -> LineBotHandler T.Text
mainHandler' (LineWebhookRequestBody dst events) = ifM ((/=) <$> pure dst <*> getBotId) unexpectedId $
    MP.mapM_ eventHandler events $> mempty
    where
        unexpectedId = $(logError) ("unexpected bot user id " <>dst)
            >> throwError (err400 { errBody = "unexpected destination" })

mainHandler :: Maybe LineSignature
    -> B.ByteString
    -> LineBotHandler T.Text
mainHandler Nothing body = $(logError) (T.decodeUtf8 body)
    >> throwError (err400 { errBody = "invalid request" })
mainHandler (Just sig) body = do
    sig' <- Base64.encode . flip hmac body <$> askLineChanSecret
    if sig' == T.encodeUtf8 sig then
        (unexpectedDecode ||| pure) (eitherDecode' $ BL.fromStrict body)
            >>= mainHandler'
    else unexpectedSig sig'
    where
        unexpectedDecode :: String -> LineBotHandler a
        unexpectedDecode s = $(logError) (tshow body)
            >> $(logError) (T.pack s)
            >> throwError (err400 { errBody = "invalid json data" })

        unexpectedSig :: B.ByteString -> LineBotHandler a
        unexpectedSig sig' = $(logError) (tshow body)
            >> $(logError) ("lhs: " <> T.decodeUtf8 sig' <> ", rhs: " <> sig)
            >> throwError (err400 { errBody = "invalid signature" })

api :: Proxy API
api = Proxy

loggingServer :: (Maybe LineSignature -> B.ByteString -> LineBotHandler T.Text)
    -> LBKiirotoriConfig
    -> Maybe LineSignature
    -> B.ByteString
    -> Handler T.Text
loggingServer f cfg s b = do
    cfg <- LineBotHandlerConfig
        <$> MySQL.newConn (cfgMySQL cfg)
        <*> Redis.newConn (cfgRedis cfg)
        <*> pure cfg
    hoistServer api (runStdoutLoggingT . flip runReaderT cfg) f s b

server :: LBKiirotoriConfig -> Server API
server = loggingServer mainHandler

kiirotoriApp :: LBKiirotoriConfig -> Application
kiirotoriApp = serve api . server

serverSettings :: Bool -> LBKiirotoriConfig -> Settings
serverSettings qFlag cfg = setPort (cfgAppPort $ cfgApp cfg)
    $ setBeforeMainLoop beforeProc defaultSettings
   where
        beforeProc = pure ()

mainServer :: (MonadIO m, MonadThrow m)
    => Bool
    -> LBKiirotoriConfig
    -> m ()
mainServer qFlag cfg = liftIO $ unless qFlag (putStrLn "ready to boot server")
    *> uncurry runSettings ((serverSettings qFlag &&& kiirotoriApp) cfg)
