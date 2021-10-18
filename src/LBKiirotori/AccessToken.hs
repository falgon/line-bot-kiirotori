{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module LBKiirotori.AccessToken (
    getAccessToken
  , getAccessTokenIO
  , getValidAccessTokenKIds
  , getValidAccessTokenKIdsIO
  , newConn
) where

import           Control.Arrow                 ((|||))
import           Control.Exception.Safe        (MonadThrow, throwString)
import           Control.Monad                 ((>=>))
import           Control.Monad.Error.Class     (MonadError)
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Crypto.JWT                    (AsError, JWTError, MonadRandom)
import qualified Data.ByteString               as BS
import           Data.Functor                  (($>))
import           Data.String                   (IsString (..))
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime)
import           Database.Redis                (Connection)

import           LBKiirotori.AccessToken.Core
import           LBKiirotori.AccessToken.Redis

getAccessToken :: forall m e. (AsError e, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => Connection
    -> m AccessToken
getAccessToken conn = takeValidToken conn >>= \case
    Just tk -> pure tk
    Nothing -> do
        r <- (runExceptT reqAccessToken :: m (Either JWTError LineIssueChannelResp))
            >>= throwString . show ||| pure
        liftIO getCurrentTime
            >>= flip (writeToken conn) r

getAccessTokenIO :: Connection -> IO AccessToken
getAccessTokenIO = f >=> throwString . show ||| pure
    where
        f :: Connection -> IO (Either JWTError AccessToken)
        f = runExceptT . getAccessToken

getValidAccessTokenKIds :: (AsError e, MonadThrow m, MonadRandom m, MonadIO m, MonadError e m)
    => m [T.Text]
getValidAccessTokenKIds = verifiedKIds <$> reqAllValidCATKIds

getValidAccessTokenKIdsIO :: IO [T.Text]
getValidAccessTokenKIdsIO = f >>= throwString . show ||| pure
    where
        f :: IO (Either JWTError [T.Text])
        f = runExceptT getValidAccessTokenKIds
