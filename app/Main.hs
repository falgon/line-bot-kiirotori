{-# LANGUAGE DeriveAnyClass, OverloadedStrings, TemplateHaskell #-}
module Main where

import           Control.Arrow                   ((&&&), (|||))
import           Control.Concurrent.Async        (concurrently)
import           Control.Exception.Safe          (throwString)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.MonoTraversable            (ointercalate)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Data.Version                    (showVersion)
import           Development.GitRev              (gitHash)
import qualified Options.Applicative             as OA
import qualified Options.Applicative.Help.Pretty as OA
import           Path                            (Dir, File, Rel)
import qualified Path                            as P
import qualified Path.IO                         as P
import qualified Paths_line_bot_kiirotori        as PR
import           System.IO                       (hFlush, stdout)
import           Text.Printf                     (printf)
import           Text.Toml                       (parseTomlDoc)

import           LBKiirotori.Config              (readConfigWithLog)
import           LBKiirotori.Internal.Utils      (getCurrentLocalTime)
import           LBKiirotori.Schedule            (watchSchedule)
import           LBKiirotori.Webhook             (mainServer)

data Cmd = CmdServe

data Opts = Opts
    { optConfigPath :: P.SomeBase P.File
    , optCronPath   :: P.SomeBase P.File
    , optQuietLog   :: Bool
    , optCmd        :: Cmd
    }

serveCmd :: OA.Mod OA.CommandFields Cmd
serveCmd = OA.command "serve"
    $ OA.info (pure CmdServe)
    $ OA.progDesc "boot server"

configPathOpt :: P.Path P.Abs P.Dir -> OA.Parser (P.SomeBase P.File)
configPathOpt homeDir = OA.option (OA.maybeReader P.parseSomeFile) $ mconcat [
    OA.long "config"
  , OA.short 'c'
  , OA.value $ P.Abs $ homeDir P.</> conf
  , OA.metavar "<config file path>"
  ]
    where
        conf = $(P.mkRelDir ".config")
            P.</> $(P.mkRelDir "lb-kiirotori")
            P.</> $(P.mkRelFile "config.toml")

cronPathOpt :: P.Path P.Abs P.Dir -> OA.Parser (P.SomeBase P.File)
cronPathOpt homeDir = OA.option (OA.maybeReader P.parseSomeFile) $ mconcat [
    OA.long "cron-schedule"
  , OA.short 's'
  , OA.value $ P.Abs $ homeDir P.</> conf
  , OA.metavar "<cron-schedule file path>"
  ]
    where
        conf = $(P.mkRelDir ".config")
            P.</> $(P.mkRelDir "lb-kiirotori")
            P.</> $(P.mkRelFile "schedule.cron")

quietLogOpt :: OA.Parser Bool
quietLogOpt = OA.switch $ mconcat [
    OA.long "quiet"
  , OA.short 'q'
  , OA.help "Quiet log output"
  ]

programOptions :: P.Path P.Abs P.Dir -> OA.Parser Opts
programOptions homeDir = Opts
    <$> configPathOpt homeDir
    <*> cronPathOpt homeDir
    <*> quietLogOpt
    <*> OA.hsubparser (mconcat [
        serveCmd
      ])

{-# INLINE logo #-}
logo :: OA.Doc
logo = OA.bold $ ointercalate OA.line [
    OA.yellow $ OA.text "　　　　 Ｖ"
  , OA.yellow $ OA.text "　　 ／￣￣￣＼　/|"
  , OA.yellow (OA.text "　　 　●＿＿●  Ｖ |") <> OA.text "     __   _ _            __             _"
  , OA.yellow (OA.text "　 ｜　 (･＿･)　 ノ") <> OA.text "    / /__(_|_)________  / /_____  _____(_)"
  , OA.yellow (OA.text "　 /　　 ヽノ　 ｜ ") <> OA.text "   / //_/ / / ___/ __ \\/ __/ __ \\/ ___/ /"
  , OA.yellow (OA.text "　(_ノ　　　　　ﾉ  ") <> OA.text "  / ,< / / / /  / /_/ / /_/ /_/ / /  / /"
  , OA.yellow (OA.text "　　＼＿＿＿＿／   ") <> OA.text " /_/|_/_/_/_/   \\____/\\__/\\____/_/  /_/"
  , OA.yellow $ OA.text "　　　 ｜　｜"
  , OA.yellow $ OA.text "　　　 个　个"
  ]

versionOption :: OA.Parser (a -> a)
versionOption = OA.infoOption vopt $ mconcat [
    OA.long "version"
  , OA.help "Prints the line-bot-kiirotori suite version"
  ]
    where
        vopt = show $ ointercalate OA.line [
            logo
          , OA.line
          , OA.text "version:" OA.<+> OA.text (showVersion PR.version)
          , OA.text "built commit hash:" OA.<+> OA.text $(gitHash)
          ]

optsParser :: P.Path P.Abs P.Dir -> OA.ParserInfo Opts
optsParser homeDir = OA.info (OA.helper <*> versionOption <*> programOptions homeDir) $ mconcat [
    OA.fullDesc
  , OA.progDesc "line-bot-kiirotori"
  ]

parseOptions :: MonadIO m => m Opts
parseOptions = P.getHomeDir
    >>= liftIO . OA.execParser . optsParser

putBootMessage :: MonadIO m => m ()
putBootMessage = do
    c <- getCurrentLocalTime
    liftIO $ OA.putDoc $ ointercalate OA.line [
        OA.underline (OA.text "version:") OA.<+> OA.bold (OA.text $ showVersion PR.version)
      , OA.underline (OA.text "commit hash:") OA.<+> OA.bold (OA.text ($(gitHash) :: String))
      , OA.hardline <> OA.text "started at" OA.<+> OA.text (show c) <> OA.hardline
      ]

main :: IO ()
main = do
    opts <- parseOptions
    cfg <- OA.putDoc (logo <> OA.hardline)
        >> putBootMessage
        >> uncurry readConfigWithLog ((optQuietLog &&& optConfigPath) opts)
    void $ concurrently
        (uncurry watchSchedule ((optQuietLog &&& optCronPath) opts) cfg)
        (mainServer (optQuietLog opts) cfg)

