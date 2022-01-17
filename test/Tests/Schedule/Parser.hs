{-# LANGUAGE OverloadedStrings #-}
module Tests.Schedule.Parser (
    tests
) where

import           LBKiirotori.Schedule.Parser (SchedulableApp (..),
                                              SchedulableAppCmd (..),
                                              SchedulableAppRow (..),
                                              parseCronSchedule)

import           Control.Exception           (SomeException)
import           Data.Either                 (fromRight)
import qualified Data.Text                   as T
import           Test.HUnit

userId :: T.Text
userId = 'U' `T.cons` T.pack (replicate 32 'a')

groupId :: T.Text
groupId = 'C' `T.cons` T.pack (replicate 32 'a')

roomId :: T.Text
roomId = 'R' `T.cons` T.pack (replicate 32 'a')

tests :: Test
tests = TestLabel "parseCronSchedule" $ TestList [
    TestLabel "push-text-message 1" $
        fromRight [] (parseCronSchedule ("* * * * * " <> userId <> " push-text-message")) ~?=
            [
                SchedulableAppRow {
                    sarCronExpr = "* * * * *"
                  , sarTargetId = userId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = []
                      }
                  }
            ]
  , TestLabel "push-text-message 2" $
        fromRight [] (parseCronSchedule ("0 * * * Wed " <> groupId <> " push-text-message abc")) ~?=
            [
                SchedulableAppRow {
                    sarCronExpr = "0 * * * Wed"
                  , sarTargetId = groupId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc" ]
                      }
                  }
            ]
  , TestLabel "push-text-message 3" $
        fromRight [] (parseCronSchedule ("0 * 3 * Tue " <> roomId <> " push-text-message abc def")) ~?=
            [
                SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc", "def" ]
                      }
                  }
            ]
  , TestLabel "push-text-message 4" $
        fromRight [] (parseCronSchedule (T.unlines [
            "0 * 3 * Tue " <> roomId <> " push-text-message abc def"
          , "0 * 3 * Tue " <> roomId <> " push-text-message abc def ghi"
          ])) ~?= [
                SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc", "def" ]
                      }
                  }
              , SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc", "def", "ghi" ]
                      }
                  }
              ]
  , TestLabel "push-text-message 5" $
        fromRight [] (parseCronSchedule (T.unlines [
            "\n"
          , "0 * 3 * Tue " <> roomId <> " push-text-message abc def # hoge"
          , "\n"
          , "0 * 3 * Tue " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * Tue " <> roomId <> " push-text-message"
          , "\n"
          , "\n"
          ])) ~?= [
                SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc", "def" ]
                      }
                  }
              , SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = [ "abc", "def", "ghi" ]
                      }
                  }
              , SchedulableAppRow {
                    sarCronExpr = "0 * 3 * Tue"
                  , sarTargetId = roomId
                  , sarApp = SchedulableApp {
                        saCmd = PushTextMessage
                      , saArg = []
                      }
                  }
              ]
  ]
