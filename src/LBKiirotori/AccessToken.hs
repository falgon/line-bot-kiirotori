module LBKiirotori.AccessToken (
    getAccessToken
  , getValidAccessTokenKIds
  , revokeCurrentAccessToken
) where

import           Control.Exception.Safe        (MonadThrow)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Parallel        (bindM2)
import           Control.Monad.Parallel        (MonadParallel)
import           Data.String                   (IsString (..))
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime)
import           Database.Redis                (Connection)

import           LBKiirotori.AccessToken.Class
import           LBKiirotori.AccessToken.Core
import           LBKiirotori.Database.Redis
import           LBKiirotori.Internal.Utils

getAccessToken :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => m AccessToken
getAccessToken = takeValidToken
    >>= maybe (bindM2 writeToken (liftIO getCurrentTime) reqAccessToken) pure

getValidAccessTokenKIds :: (MonadIO m, MonadThrow m, MonadParallel m, AccessTokenMonad m)
    => m [T.Text]
getValidAccessTokenKIds = verifiedKIds <$> reqAllValidCATKIds

revokeCurrentAccessToken :: (MonadIO m, MonadThrow m, AccessTokenMonad m)
    => m ()
revokeCurrentAccessToken = takeToken
    >>= maybe (pure ()) reqRevokeChannelAccess
