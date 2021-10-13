{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module LBKiirotori.AccessToken (
    getAccessToken
  , getAccessTokenIO
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
    -> m BS.ByteString
getAccessToken conn = takeValidToken conn >>= \case
    Just tk -> pure tk
    Nothing -> do
        r <- (runExceptT reqAccessToken :: m (Either JWTError LineReqResp))
            >>= throwString . show ||| pure
        t <- liftIO getCurrentTime
        writeToken conn t r
            $> fromString (T.unpack $ accessToken r)

getAccessTokenIO :: Connection -> IO BS.ByteString
getAccessTokenIO = f >=> throwString . show ||| pure
    where
        f :: Connection -> IO (Either JWTError BS.ByteString)
        f = runExceptT . getAccessToken
