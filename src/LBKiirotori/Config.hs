{-# LANGUAGE CPP, DerivingStrategies, GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
module LBKiirotori.Config (
    LBKiirotoriConfig (..)
  , LBKiirotoriAppConfig (..)
  , LBKiirotoriLineConfig (..)
  , readConfig
) where

import           Control.Arrow                  ((|||))
import           Control.Exception.Safe         (MonadThrow (..), throwString)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Logger           (LoggingT)
import           Control.Monad.Reader           (ReaderT, asks)
import qualified Data.ByteString                as B
import           Data.Functor                   ((<&>))
import qualified Data.HashMap.Lazy              as HM
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import           Database.Redis                 (Connection)
import qualified Path                           as P
import           Servant.Server                 (Handler)
import           Text.Toml                      (parseTomlDoc)
import           Text.Toml.Types                (Node (..), Table)

import           LBKiirotori.AccessToken        (getAccessTokenIO)
import           LBKiirotori.AccessToken.Config (AccessToken (..))

newtype LBKiirotoriAppConfig = LBKiirotoriAppConfig {
    cfgAppWelcome :: T.Text
  }
  deriving stock Show
#ifndef RELEASE
  deriving newtype (Semigroup, Monoid)
#endif

data LBKiirotoriLineConfig = LBKiirotoriLineConfig {
    cfgJWKSet        :: B.ByteString
  , cfgKID           :: B.ByteString
  , cfgChannelID     :: B.ByteString
  , cfgChannelSecret :: B.ByteString
  , cfgUserID        :: B.ByteString
  }
  deriving stock Show

#ifndef RELEASE
instance Semigroup LBKiirotoriLineConfig where
    (LBKiirotoriLineConfig l1 l2 l3 l4 l5) <> (LBKiirotoriLineConfig r1 r2 r3 r4 r5) =
        LBKiirotoriLineConfig (l1 <> r1) (l2 <> r2) (l3 <> r3) (l4 <> r4) (l5 <> r5)

instance Monoid LBKiirotoriLineConfig where
    mempty = LBKiirotoriLineConfig mempty mempty mempty mempty mempty
#endif

data LBKiirotoriConfig = LBKiirotoriConfig {
    cfgApp  :: LBKiirotoriAppConfig
  , cfgLine :: LBKiirotoriLineConfig
  }
  deriving stock Show

#ifndef RELEASE
instance Semigroup LBKiirotoriConfig where
    (LBKiirotoriConfig l1 l2) <> (LBKiirotoriConfig r1 r2) =
        LBKiirotoriConfig (l1 <> r1) (l2 <> r2)

instance Monoid LBKiirotoriConfig where
    mempty = LBKiirotoriConfig mempty mempty
#endif

readToml :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m Table
readToml fp = liftIO (T.readFile fpath)
    >>= ((throwString . show) ||| pure) . parseTomlDoc fpath
    where
        fpath = P.fromSomeFile fp

lookupTable :: MonadThrow m
    => T.Text
    -> Table
    -> m Table
lookupTable key tb = case HM.lookup key tb of
    Just (VTable tb) -> pure tb
    _ -> throwString $ "expected table [" <> T.unpack key <> "]"

lookupString :: (IsString s, MonadThrow m)
    => T.Text
    -> Table
    -> m s
lookupString key tb = case HM.lookup key tb of
    Just (VString s) -> pure $ fromString $ T.unpack s
    _                -> throwString $ "expected string " <> T.unpack key

readConfig :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m LBKiirotoriConfig
readConfig fp = do
    tables <- readToml fp
    appConfig <- lookupTable "app" tables
        >>= lookupString "welcome_message"
        <&> LBKiirotoriAppConfig
    lineTable <- lookupTable "line" tables
    lineConfig <- LBKiirotoriLineConfig
        <$> (lookupString "jwk_set_key_path" lineTable >>= liftIO . B.readFile)
        <*> lookupString "kid" lineTable
        <*> lookupString "channel_id" lineTable
        <*> lookupString "channel_secret" lineTable
        <*> lookupString "user_id" lineTable
    pure $ LBKiirotoriConfig appConfig lineConfig
