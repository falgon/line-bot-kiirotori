module LBKiirotori.Webhook.EventObject.LineBotHandler (
    LineBotHandlerConfig (..)
  , LineBotHandler
  , askLineKId
  , askLineChanId
  , askLineChanSecret
  , askLineUserId
  , askAccessToken
) where

import           Control.Arrow                  ((|||))
import           Control.Exception.Safe         (MonadThrow (..), throwString)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Logger           (LoggingT)
import           Control.Monad.Reader           (ReaderT, asks)
import qualified Data.ByteString                as B
import           Data.Functor                   ((<&>))
import qualified Data.HashMap.Lazy              as HM
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import           Database.Redis                 (Connection)
import qualified Path                           as P
import           Servant.Server                 (Handler)
import           Text.Toml                      (parseTomlDoc)
import           Text.Toml.Types                (Node (..), Table)

import           LBKiirotori.AccessToken        (getAccessTokenIO)
import           LBKiirotori.AccessToken.Config (AccessToken (..))
import           LBKiirotori.Config             (LBKiirotoriAppConfig (..),
                                                 LBKiirotoriConfig (..),
                                                 LBKiirotoriLineConfig (..))

data LineBotHandlerConfig = LineBotHandlerConfig {
    lbhRedisConn :: Connection
  , lbhCfg       :: LBKiirotoriConfig
  }

type LineBotHandler = ReaderT LineBotHandlerConfig (LoggingT Handler)

askLineKId :: LineBotHandler B.ByteString
askLineKId = asks $ cfgKID . cfgLine . lbhCfg

askLineChanId :: LineBotHandler B.ByteString
askLineChanId = asks $ cfgChannelID . cfgLine . lbhCfg

askLineChanSecret :: LineBotHandler B.ByteString
askLineChanSecret = asks $ cfgChannelSecret . cfgLine . lbhCfg

askLineUserId :: LineBotHandler B.ByteString
askLineUserId = asks $ cfgUserID . cfgLine . lbhCfg

askAccessToken :: LineBotHandler AccessToken
askAccessToken = asks lbhRedisConn
    >>= liftIO . getAccessTokenIO
