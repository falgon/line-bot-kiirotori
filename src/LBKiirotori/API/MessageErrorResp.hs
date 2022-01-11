{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module LBKiirotori.API.MessageErrorResp (
    MessageErrorResp (..)
) where

import           Control.Exception.Safe (Exception)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text              as T
import           GHC.Generics

newtype MessageErrorResp = MessageErrorResp {
    merMessage :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON MessageErrorResp where
    parseJSON (Object v) = MessageErrorResp
        <$> v .: "message"
    parseJSON invalid = prependFailure "parsing MessageErrorResp failed, "
        $ typeMismatch "Object" invalid

instance Exception MessageErrorResp
