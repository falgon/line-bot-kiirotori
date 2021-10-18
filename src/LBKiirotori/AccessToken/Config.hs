module LBKiirotori.AccessToken.Config (
    getChannelID
  , getChannelSecret
  , AccessToken (..)
) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import           System.Environment     (getEnv)

getChannelID :: (MonadIO m, IsString s) => m s
getChannelID = liftIO $ fromString <$> getEnv channelIDEnv
    where
        channelIDEnv = "LINE_CHANNEL_ID"

getChannelSecret :: (MonadIO m, IsString s) => m s
getChannelSecret = liftIO $ fromString <$> getEnv channelSecretEnv
    where
        channelSecretEnv = "LINE_CHANNEL_SECRET"

data AccessToken = AccessToken {
    atKeyId     :: T.Text
  , atToken     :: BS.ByteString
  , atExpiresIn :: UTCTime
  } deriving Show
