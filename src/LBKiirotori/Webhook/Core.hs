{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module LBKiirotori.Webhook.Core (
    mainServer
) where

import           Control.Arrow                                   ((&&&), (|||))
import           Control.Exception.Safe                          (MonadThrow)
import           Control.Monad                                   (forM_, unless)
import           Control.Monad.Except                            (MonadError (..))
import           Control.Monad.Extra                             (ifM)
import           Control.Monad.IO.Class                          (MonadIO (..))
import           Control.Monad.Logger                            (LoggingT (..),
                                                                  logError,
                                                                  logInfo,
                                                                  runStdoutLoggingT)
import           Control.Monad.Parallel                          as MP
import           Control.Monad.Reader                            (ReaderT (..),
                                                                  asks)
import           Control.Monad.Trans.Maybe                       (MaybeT (..))
import           Crypto.Hash.SHA256                              (hmac)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                                 as B
import qualified Data.ByteString.Base64                          as Base64
import qualified Data.ByteString.Lazy                            as BL
import           Data.Functor                                    (($>))
import qualified Data.HashMap.Strict                             as HM
import qualified Data.List.NonEmpty                              as NE
import           Data.Maybe                                      (fromMaybe)
import           Data.String                                     (IsString (..))
import qualified Data.Text                                       as T
import qualified Data.Text.Encoding                              as T
import qualified Data.Vector                                     as V
import qualified Network.HTTP.Media                              as M
import           Network.Wai.Handler.Warp                        (Port,
                                                                  Settings,
                                                                  defaultSettings,
                                                                  runSettings,
                                                                  setBeforeMainLoop,
                                                                  setPort)
import qualified Options.Applicative.Help.Pretty                 as OA
import qualified Path                                            as P
import           Servant
import           Servant.API.ContentTypes                        (Accept (..))
import           Servant.Server                                  (Application,
                                                                  Handler (..),
                                                                  Server,
                                                                  hoistServer,
                                                                  serve)
import           Servant.Server.Internal.ServerError

import           LBKiirotori.AccessToken                         (getAccessToken)
import           LBKiirotori.API.PushMessage
import           LBKiirotori.BotProfile                          (getBotUserId)
import           LBKiirotori.Config                              (LBKiirotoriAppConfig (..),
                                                                  LBKiirotoriConfig (..),
                                                                  readConfigWithLog)
import           LBKiirotori.Data.MessageObject                  (MessageBody (..),
                                                                  textMessage)
import qualified LBKiirotori.Database.MySQL                      as MySQL
import qualified LBKiirotori.Database.Redis                      as Redis
import           LBKiirotori.Internal.Utils                      (hoistMaybe,
                                                                  tshow)
import           LBKiirotori.Webhook.EventHandlers
import           LBKiirotori.Webhook.EventObject
import           LBKiirotori.Webhook.EventObject.LineBotHandler

import           LBKiirotori.Webhook.EventHandlers.Ext.SendPlain

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#request-body
data RequestBody e = RequestBody {
    rbDst    :: T.Text
  , rbEvents :: [e]
  } deriving Show

instance FromJSON e => FromJSON (RequestBody e) where
    parseJSON (Object v) = RequestBody
        <$> v .: "destination"
        <*> v .: "events"

instance ToJSON e => ToJSON (RequestBody e) where
    toJSON v = Object $ HM.fromList [
        ("destination", String $ rbDst v)
      , ("events", toJSON $ rbEvents v)
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
type LINEBotAPI = "linebot"
    :> "webhook"
    :> Header "x-line-signature" T.Text
    :> ReqBody '[WebhookJSON] B.ByteString
    :> Post '[PlainText] T.Text

type LINEExtBotAPI = "linebot"
    :> "ext-webhook"
    :> Header "x-ext-line-signature" T.Text
    :> ReqBody '[WebhookJSON] B.ByteString
    :> Post '[PlainText] T.Text

handleBot :: forall e. (FromJSON e, EventHandler e)
    => Proxy e
    -> Maybe T.Text
    -> B.ByteString
    -> LineBotHandler T.Text
handleBot proxy Nothing body = $(logError) (T.decodeUtf8 body)
    >> throwError (err400 { errBody = "invalid request" })
handleBot proxy (Just sig) body = do
    sig' <- Base64.encode . flip hmac body <$> askLineChanSecret
    if sig' /= T.encodeUtf8 sig then unexpectedSig sig' else do
        reqBody <- (unexpectedDecode ||| pure) (eitherDecode' (BL.fromStrict body) :: Either String (RequestBody e))
        ifM ((/=) <$> pure (rbDst reqBody) <*> getBotUserId) (unexpectedId reqBody) $
            MP.mapM_ handle (rbEvents reqBody) $> mempty
    where
        unexpectedSig :: B.ByteString -> LineBotHandler a
        unexpectedSig sig' = $(logError) (tshow body)
            >> $(logError) ("lhs: " <> T.decodeUtf8 sig' <> ", rhs: " <> sig)
            >> throwError (err400 { errBody = "invalid signature" })

        unexpectedDecode :: String -> LineBotHandler a
        unexpectedDecode s = $(logError) (tshow body)
            >> $(logError) (T.pack s)
            >> throwError (err400 { errBody = "invalid json data" })

        unexpectedId :: RequestBody e -> LineBotHandler a
        unexpectedId reqBody = $(logError) ("unexpected bot user id " <> rbDst reqBody)
            >> throwError (err400 { errBody = "unexpected destination" })

handleLineBot :: Maybe T.Text
    -> B.ByteString
    -> LineBotHandler T.Text
handleLineBot = handleBot (Proxy @ LineEventObject)

handleExtBot :: Maybe T.Text
    -> B.ByteString
    -> LineBotHandler T.Text
handleExtBot = handleBot (Proxy @ ExtEventObject)

type API = LINEBotAPI :<|> LINEExtBotAPI

connectConfig :: LBKiirotoriConfig
    -> Handler LineBotHandlerConfig
connectConfig cfg = LineBotHandlerConfig
    <$> MySQL.newConn (cfgMySQL cfg)
    <*> Redis.newConn (cfgRedis cfg)
    <*> pure cfg

lineBotServer :: LBKiirotoriConfig
    -> Maybe T.Text
    -> B.ByteString
    -> Handler T.Text
lineBotServer cfg s b = do
    cfg' <- connectConfig cfg
    hoistServer (Proxy @ LINEBotAPI) (runStdoutLoggingT . flip runReaderT cfg') handleLineBot s b

lineExtBotServer :: LBKiirotoriConfig
    -> Maybe T.Text
    -> B.ByteString
    -> Handler T.Text
lineExtBotServer cfg s b = do
    cfg' <- connectConfig cfg
    hoistServer (Proxy @ LINEExtBotAPI) (runStdoutLoggingT . flip runReaderT cfg') handleExtBot s b

server :: LBKiirotoriConfig -> Server API
server cfg = lineBotServer cfg :<|> lineExtBotServer cfg

kiirotoriApp :: LBKiirotoriConfig -> Application
kiirotoriApp = serve (Proxy @ API) . server

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
