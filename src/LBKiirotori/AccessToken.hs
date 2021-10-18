{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module LBKiirotori.AccessToken (
    getAccessToken
  , getAccessTokenIO
  , getValidAccessTokenKIds
  , getValidAccessTokenKIdsIO
  , revokeCurrentAccessToken
  , newConn
) where

import           Control.Arrow                  ((|||))
import           Control.Exception.Safe         (MonadThrow, throwString)
import           Control.Monad                  ((>=>))
import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.Except           (runExceptT)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Parallel         (MonadParallel, bindM2)
import           Crypto.JWT                     (AsError, JWTError, MonadRandom)
import qualified Data.ByteString                as BS
import           Data.Functor                   (($>))
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import           Data.Time.Clock                (getCurrentTime)
import           Database.Redis                 (Connection)

import           LBKiirotori.AccessToken.Config (getChannelID, getChannelSecret)
import           LBKiirotori.AccessToken.Core
import           LBKiirotori.AccessToken.Redis

getAccessToken :: forall m e. (AsError e, MonadParallel m, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => Connection
    -> m AccessToken
getAccessToken conn = takeValidToken conn >>= \case
    Just tk -> pure tk
    Nothing -> bindM2 (writeToken conn)
        (liftIO getCurrentTime)
        (f >>= throwString . show ||| pure)
    where
        f :: m (Either JWTError LineIssueChannelResp)
        f = runExceptT reqAccessToken

getAccessTokenIO :: Connection -> IO AccessToken
getAccessTokenIO = f >=> throwString . show ||| pure
    where
        f :: Connection -> IO (Either JWTError AccessToken)
        f = runExceptT . getAccessToken

getValidAccessTokenKIds :: (AsError e, MonadParallel m, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => m [T.Text]
getValidAccessTokenKIds = verifiedKIds <$> reqAllValidCATKIds

getValidAccessTokenKIdsIO :: IO [T.Text]
getValidAccessTokenKIdsIO = f >>= throwString . show ||| pure
    where
        f :: IO (Either JWTError [T.Text])
        f = runExceptT getValidAccessTokenKIds

revokeCurrentAccessToken :: (MonadParallel m, MonadThrow m, MonadIO m)
    => Connection
    -> m ()
revokeCurrentAccessToken conn = takeToken conn >>= \case
    Nothing -> pure ()
    Just tk -> bindM2 (reqRevokeChannelAccess tk) getChannelID getChannelSecret

