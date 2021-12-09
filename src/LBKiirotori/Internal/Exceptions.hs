{-# LANGUAGE CPP, ExplicitNamespaces, FlexibleContexts, ImplicitParams,
             Rank2Types, TypeOperators #-}
module LBKiirotori.Internal.Exceptions (
    invalidArgument
  , MonadThrowable (..)
  , stringException
  , liftMTE
) where

import           Control.Arrow             ((|||))
import           Control.Exception         (IOException)
import           Control.Exception.Safe    (Exception, MonadThrow,
                                            StringException (..), throw,
                                            throwString)
import           Control.Monad             ((>=>))
import           Control.Monad.Error       (ErrorT, runErrorT)
import           Control.Monad.Except      (ExceptT, runExceptT)
import           Control.Monad.Identity    (IdentityT (..))
import           Control.Monad.List        (ListT (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Natural           (type (~>))
import           Crypto.JWT                (JWTError)
import           Data.Functor.Identity     (Identity (..))
import           GHC.IO.Exception          (IOErrorType (..))
import           System.IO.Error           (mkIOError)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack.Types           (HasCallStack)
#endif

invalidArgument :: String -> IOError
invalidArgument desc = mkIOError InvalidArgument desc Nothing Nothing

class MonadThrowable m where
    fromMonad :: (MonadThrow n, Exception e) => Maybe e -> m ~> n

instance MonadThrowable Maybe where
    fromMonad Nothing  = throwString "Nothing" `maybe` pure
    fromMonad (Just e) = throw e `maybe` pure

instance MonadThrowable Identity where
    fromMonad _ = pure . runIdentity

instance Exception e => MonadThrowable (Either e) where
    fromMonad Nothing  = throw ||| pure
    fromMonad (Just e) = const (throw e) ||| pure

instance MonadThrowable [] where
    fromMonad Nothing []  = throwString "empty"
    fromMonad (Just e) [] = throw e
    fromMonad _ (x:_)     = pure x

instance (Exception e, MonadThrowable m) => MonadThrowable (ExceptT e m) where
    fromMonad e = fromMonad e . runExceptT >=> fromMonad e

instance MonadThrowable m => MonadThrowable (MaybeT m) where
    fromMonad e = fromMonad e . runMaybeT >=> fromMonad e

instance MonadThrowable m => MonadThrowable (IdentityT m) where
    fromMonad e = fromMonad e . runIdentityT

instance MonadThrowable m => MonadThrowable (ListT m) where
    fromMonad e = fromMonad e . runListT >=> fromMonad e

instance (Exception e, MonadThrowable m) => MonadThrowable (ErrorT e m) where
    fromMonad e = fromMonad e . runErrorT >=> fromMonad e

#if MIN_VERSION_base(4,9,0)
stringException :: HasCallStack => String -> StringException
stringException s = StringException s ?callStack
#else
stringException :: String -> StringException
stringException s = StringException s ()
#endif

instance Exception JWTError

liftMTE :: MonadThrow m => Either String c -> m c
liftMTE = throwString ||| pure
