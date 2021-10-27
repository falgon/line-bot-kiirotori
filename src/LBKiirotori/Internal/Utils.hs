module LBKiirotori.Internal.Utils (
    tshow
  , stripFirstToLowerLabeledOption
) where

import           Data.Aeson
import           Data.Char        (toLower)
import qualified Data.Text        as T
import           Data.Tuple.Extra (dupe, first, second)

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
