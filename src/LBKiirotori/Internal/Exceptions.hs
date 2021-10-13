module LBKiirotori.Internal.Exceptions (
    invalidArgument
) where

import           GHC.IO.Exception (IOErrorType (..))
import           System.IO.Error  (mkIOError)

invalidArgument :: String -> IOError
invalidArgument desc = mkIOError InvalidArgument desc Nothing Nothing

