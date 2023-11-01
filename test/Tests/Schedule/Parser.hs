{-# LANGUAGE OverloadedStrings #-}
module Tests.Schedule.Parser (
    tests
) where

import           LBKiirotori.Schedule.Parser

import           Control.Exception           (SomeException)
import           Data.Either                 (fromRight)
import qualified Data.Text                   as T
import qualified System.Cron                 as C
import           Test.HUnit

userId :: T.Text
userId = 'U' `T.cons` T.pack (replicate 32 'a')

groupId :: T.Text
groupId = 'C' `T.cons` T.pack (replicate 32 'a')

roomId :: T.Text
roomId = 'R' `T.cons` T.pack (replicate 32 'a')

cronSchedule :: T.Text -> C.CronSchedule
cronSchedule t = case C.parseCronSchedule t of
    Right x -> x
    Left _  -> error "invalid schedule"

tests :: Test
tests = TestLabel "parseSchedule" $ TestList [
    TestLabel "push-text-message 1" $
        fromRight [] (parseSchedule ("* * * * * " <> userId <> " push-text-message")) ~?=
            [
                ScheduleEntry {
                    seSchedule = cronSchedule "* * * * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = userId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = []
                          }
                      }
                  }
            ]
  , TestLabel "push-text-message 2" $
        fromRight [] (parseSchedule ("0 * * * * " <> groupId <> " push-text-message abc")) ~?=
            [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * * * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = groupId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc" ]
                          }
                      }
                  }
            ]
  , TestLabel "push-text-message 3" $
        fromRight [] (parseSchedule ("0 * 3 * * " <> roomId <> " push-text-message abc def")) ~?=
            [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
            ]
  , TestLabel "push-text-message 4" $
        fromRight [] (parseSchedule (T.unlines [
            "0 * 3 * * " <> roomId <> " push-text-message abc def"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 5" $
        fromRight [] (parseSchedule (T.unlines [
            "# this is comment"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def"
          , "\n"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message"
          , "\n"
          , "\n"
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = []
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 6" $
        fromRight [] (parseSchedule (T.unlines [
            "# variable test"
          , "ROOMID=" <> roomId
          , "0 * 3 * * $ROOMID push-text-message abc def"
          , "0 * 3 * * ${ROOMID} push-text-message abc def ghi"
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 7" $
        fromRight [] (parseSchedule (T.unlines [
            "# variable test"
          , "ROOMID=" <> roomId
          , "command=push-text-message"
          , "0 * 3 * * $ROOMID $command abc def"
          , "0 * 3 * * ${ROOMID} $command abc def \"ghi jk\""
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi jk" ]
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 8" $
        fromRight [] (parseSchedule (T.unlines [
            "# variable test"
          , "ROOMID=" <> roomId
          , "command=push-text-message"
          , "arg1=abc"
          , "arg2=\"def\""
          , "arg3=\"defghi\""
          , "0 * 3 * * $ROOMID $command $arg1 $arg2"
          , "0 * 3 * * ${ROOMID} $command $arg1 $arg3"
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "defghi" ]
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 9" $
        fromRight [] (parseSchedule (T.unlines [
            "0 * 3 * * " <> roomId <> " push-text-message abc def"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          , "0 * 3 * * " <> roomId <> " push-text-message abc def ghi"
          ])) ~?= [
                ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "abc", "def", "ghi" ]
                          }
                      }
                  }
              ]
  , TestLabel "push-text-message 10" $
        fromRight [] (parseSchedule (T.unlines [
            "0 * 3 * * " <> roomId <> " push-text-message 'あいうえお🔥🗑️🔥🗑️'"
          , "0 * 3 * * " <> roomId <> " push-text-message あいうえお🔥🗑️🔥🗑️"
          ])) ~?= [
               ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "あいうえお🔥🗑️🔥🗑️" ]
                          }
                      }
                  }
             , ScheduleEntry {
                    seSchedule = cronSchedule "0 * 3 * *"
                  , seTargetSchedule = TargetSchedule {
                        saTargetId = roomId
                      , saApp = SchedulableApp {
                            saCmd = PushTextMessage
                          , saArg = [ "あいうえお🔥🗑️🔥🗑️" ]
                          }
                      }
                  }
              ]
  ]
