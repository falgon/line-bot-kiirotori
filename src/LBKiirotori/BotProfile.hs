module LBKiirotori.BotProfile (
    getBotUserId
) where

import           Control.Exception.Safe        (MonadThrow)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Parallel        (MonadParallel)
import qualified Data.Text                     as T

import           LBKiirotori.AccessToken
import           LBKiirotori.AccessToken.Class
import           LBKiirotori.API.Profile.Bot
import           LBKiirotori.Database.Redis
import           LBKiirotori.Internal.Utils    (fromMaybeM)

getBotUserId :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => m T.Text
getBotUserId = takeBotUserId
    >>= fromMaybeM (getAccessToken >>= reqProfileBot >>= writeBotUserId)

