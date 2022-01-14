module Main where

import           Test.Hspec               (parallel)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.Hspec.Core.Runner   (Config (..), defaultConfig,
                                           evaluateSummary, runSpec)
import           Test.HUnit

import qualified Tests.Schedule.Parser    as SP

testMain :: Test
testMain = TestLabel "line-bot-kiirotori" $ TestList [
    TestLabel "schedule" $ TestList [
        TestLabel "parser" $ TestList [
            SP.tests
          ]
        ]
      ]

main :: IO ()
main = runSpec
    (parallel $ fromHUnitTest testMain)
    (defaultConfig { configPrintCpuTime = True })
        >>= evaluateSummary
