module LBKiirotori.Schedule.Parser (

) where

import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified System.Cron.Schedule as C

import LBKiirotori.Config (LBKiirotoriConfig)

data ScheduledApp m = ScheduledApp {
    saCronStatement :: T.Text
  , saApp :: ScheduleT m
  }
  deriving (Show, Eq)



