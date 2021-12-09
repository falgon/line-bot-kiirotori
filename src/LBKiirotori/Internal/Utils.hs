module LBKiirotori.Internal.Utils (
    tshow
  , stripFirstToLowerLabeledOption
  , decodeJSON
) where

import           Control.Arrow                   ((|||))
import           Control.Exception.Safe          (MonadThrow (..), throwString)
import           Data.Aeson                      
import qualified Data.ByteString.Lazy            as BL
import           Data.Char                       (toLower)
import qualified Data.Text                       as T
import           Data.Tuple.Extra                (dupe, first, second)

import           LBKiirotori.Internal.Exceptions (liftMTE)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

firstToLowerLabelModifier :: Int -> String -> String
firstToLowerLabelModifier n = uncurry (:)
    . first (toLower . head)
    . second tail
    . dupe . drop n

stripFirstToLowerLabeledOption :: Int -> Options
stripFirstToLowerLabeledOption n = defaultOptions {
    fieldLabelModifier = firstToLowerLabelModifier n
  }

decodeJSON :: (MonadThrow m, FromJSON a) => BL.ByteString -> m a
decodeJSON = (throwString ||| pure) . eitherDecode
