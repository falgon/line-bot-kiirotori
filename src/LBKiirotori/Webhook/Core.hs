{-# LANGUAGE DataKinds, MultiParamTypeClasses, OverloadedStrings,
             TemplateHaskell, TypeOperators #-}
module LBKiirotori.Webhook.Core (
    kiirotoriApp
) where

import           Control.Arrow                       ((|||))
import           Control.Monad.Except                (MonadError (..))
import           Control.Monad.IO.Class              (MonadIO (..))
import           Control.Monad.Logger                (LoggingT, logError,
                                                      logInfo,
                                                      runStdoutLoggingT)
import           Control.Monad.Reader                (ReaderT (..), asks)
import           Crypto.Hash.SHA256                  (hmac)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Base64              as Base64
import qualified Data.ByteString.Lazy                as BL
import qualified Data.ByteString.Lazy                as BL
import           Data.Functor                        (($>))
import qualified Data.HashMap.Strict                 as HM
import qualified Data.List.NonEmpty                  as NE
import           Data.String                         (IsString (..))
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Vector                         as V
import qualified Network.HTTP.Media                  as M
import           Servant                             (Header, JSON,
                                                      MimeRender (..),
                                                      MimeUnrender (..),
                                                      PlainText, Post,
                                                      Proxy (..), ReqBody,
                                                      type (:>))
import           Servant.API.ContentTypes            (Accept (..))
import           Servant.Server                      (Application, Handler,
                                                      Server, hoistServer,
                                                      serve)
import           Servant.Server.Internal.ServerError

import           LBKiirotori.AccessToken.Config      (getChannelSecret)
import           LBKiirotori.Internal.Utils          (tshow)
import           LBKiirotori.Webhook.EventObject

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
    mimeUnrender _ = Right . id

instance MimeUnrender WebhookJSON B.ByteString where
    mimeUnrender _ = Right . BL.toStrict

-- c.f. https://developers.line.biz/ja/reference/messaging-api/#request-headers
type API = "linebot"
    :> "webhook"
    :> Header "x-line-signature" LineSignature
    :> ReqBody '[WebhookJSON] B.ByteString
    :> Post '[PlainText] T.Text

newtype LineBotHandlerConfig = LineBotHandlerConfig {
    lbhChannelSecret :: B.ByteString
  } deriving Show

type LineBotHandler = ReaderT LineBotHandlerConfig (LoggingT Handler)

mainHandler' :: LineWebhookRequestBody
    -> LineBotHandler T.Text
mainHandler' body = $(logInfo) (tshow body)
    $> mempty

mainHandler :: Maybe LineSignature
    -> B.ByteString
    -> LineBotHandler T.Text
mainHandler Nothing body = $(logError) (T.decodeUtf8 body)
    >> throwError (err400 { errBody = "invalid request" })
mainHandler (Just sig) body = do
    sig' <- asks (Base64.encode . flip hmac body . lbhChannelSecret)
    if sig' == T.encodeUtf8 sig then
        (unexpectedDecode ||| pure) (eitherDecode' $ BL.fromStrict body)
            >>= mainHandler'
    else unexpectedSig sig'
    where
        unexpectedDecode :: String -> LineBotHandler a
        unexpectedDecode s = $(logError) (tshow body)
            >> throwError (err400 { errBody = fromString s })

        unexpectedSig :: B.ByteString -> LineBotHandler a
        unexpectedSig sig' = $(logError) (tshow body)
            >> $(logError) ("lhs: " <> T.decodeUtf8 sig' <> ", rhs: " <> sig)
            >> throwError (err400 { errBody = "invalid signature" })

api :: Proxy API
api = Proxy

loggingServer :: (Maybe LineSignature -> B.ByteString -> LineBotHandler T.Text)
    -> Maybe LineSignature
    -> B.ByteString
    -> Handler T.Text
loggingServer f s b = do
    cfg <- liftIO $ LineBotHandlerConfig <$> getChannelSecret
    hoistServer api (runStdoutLoggingT . flip runReaderT cfg) f s b

server :: Server API
server = loggingServer mainHandler

kiirotoriApp :: Application
kiirotoriApp = serve api server
