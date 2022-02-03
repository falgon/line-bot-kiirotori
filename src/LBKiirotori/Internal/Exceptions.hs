{-# LANGUAGE CPP, ExplicitNamespaces, FlexibleContexts, ImplicitParams,
             TypeOperators #-}
module LBKiirotori.Internal.Exceptions (
    invalidArgument
  , stringException
) where

import           Control.Arrow          ((|||))
import           Control.Exception      (IOException)
import           Control.Exception.Safe (Exception, MonadThrow,
                                         StringException (..), throw,
                                         throwString)
import           Crypto.JWT             (JWTError)
import           GHC.IO.Exception       (IOErrorType (..))
import           System.IO.Error        (mkIOError)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack.Types        (HasCallStack)
#endif

invalidArgument :: String -> IOError
invalidArgument desc = mkIOError InvalidArgument desc Nothing Nothing

#if MIN_VERSION_base(4,9,0)
stringException :: HasCallStack => String -> StringException
stringException s = StringException s ?callStack
#else
stringException :: String -> StringException
stringException s = StringException s ()
#endif

instance Exception JWTError
