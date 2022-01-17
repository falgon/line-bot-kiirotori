{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module LBKiirotori.Internal.Utils (
    tshow
  , stripFirstToLowerLabeledOption
  , decodeJSON
  , hoistMaybe
  , fromMaybeM
  , utcToCurrentLocalTimeZone
  , localTimeToCurrentUTCTimeZone
  , getCurrentLocalTime
  , doubleToUTCTime
  , mapSomeBase
) where

import           Control.Arrow             ((|||))
import           Control.Exception.Safe    (MonadThrow (..), throwString)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Logger      (LoggingT (..))
import           Control.Monad.Parallel    (MonadParallel (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (toLower)
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX     (posixSecondsToUTCTime)
import           Data.Time.LocalTime       (LocalTime, getCurrentTimeZone,
                                            localTimeToUTC, utcToLocalTime)
import           Data.Tuple.Extra          (dupe, first, second)
import qualified Path                      as P
import           Servant.Server            (Handler (..))

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

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM = flip maybe pure

utcToCurrentLocalTimeZone :: MonadIO m => UTCTime -> m LocalTime
utcToCurrentLocalTimeZone utct = utcToLocalTime
    <$> liftIO getCurrentTimeZone
    <*> pure utct

localTimeToCurrentUTCTimeZone :: MonadIO m => LocalTime -> m UTCTime
localTimeToCurrentUTCTimeZone lt = localTimeToUTC
    <$> liftIO getCurrentTimeZone
    <*> pure lt

getCurrentLocalTime :: MonadIO m => m LocalTime
getCurrentLocalTime = liftIO getCurrentTime
    >>= utcToCurrentLocalTimeZone

doubleToUTCTime :: Double -> UTCTime
doubleToUTCTime = posixSecondsToUTCTime . realToFrac

-- It has existed since path library 0.9.1, but since 0.8.0 is used in this project.
-- c.f. https://github.com/commercialhaskell/path/blob/master/CHANGELOG#L7
mapSomeBase :: (forall b. P.Path b t -> P.Path b t') -> P.SomeBase t -> P.SomeBase t'
mapSomeBase f = \case
    P.Abs a -> P.Abs $ f a
    P.Rel r -> P.Rel $ f r

instance MonadParallel m => MonadParallel (LoggingT m) where
    bindM2 f ma mb = LoggingT $ \g ->
        bindM2 ((.) (`runLoggingT` g) . f) (runLoggingT ma g) (runLoggingT mb g)

instance MonadParallel Handler where
    bindM2 f ma mb = Handler $
        bindM2 ((.) runHandler' . f) (runHandler' ma) (runHandler' mb)
