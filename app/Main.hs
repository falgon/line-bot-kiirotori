{-# LANGUAGE DeriveAnyClass, OverloadedStrings, TemplateHaskell #-}
module Main where

import           Control.Arrow            ((|||))
import           Control.Exception.Safe   (throwString)
import           Control.Monad.IO.Class   (MonadIO (..))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Network.Wai.Handler.Warp (run)
import qualified Options.Applicative      as OA
import           Path                     (Dir, File, Rel)
import qualified Path                     as P
import qualified Path.IO                  as P
import           Text.Toml                (parseTomlDoc)

import           LBKiirotori.Config       (readConfig)
import           LBKiirotori.Webhook      (kiirotoriApp)

data Cmd = CmdServe

data Opts = Opts
    { optConfigPath :: P.SomeBase P.File
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

programOptions :: P.Path P.Abs P.Dir -> OA.Parser Opts
programOptions homeDir = Opts
    <$> configPathOpt homeDir
    <*> OA.hsubparser (mconcat [
        serveCmd
      ])

optsParser :: P.Path P.Abs P.Dir -> OA.ParserInfo Opts
optsParser homeDir = OA.info (OA.helper <*> programOptions homeDir) $ mconcat [
    OA.fullDesc
  , OA.progDesc "line-bot-kiirotori"
  ]

parseOptions :: MonadIO m => m Opts
parseOptions = P.getHomeDir
    >>= liftIO . OA.execParser . optsParser

main :: IO ()
main = do
    opts <- parseOptions
    putStrLn "boot server..."
        >> readConfig (optConfigPath opts)
        >>= run 48080 . kiirotoriApp

{-
    conn <- newConn
    token <- getAccessTokenIO conn
    userId <- getEnv "USER_ID"
    pushMessage token $ PushMessage {
        to = T.pack userId
      , messages = [
            MBText $ textMessage "pi!" Nothing Nothing
          ]
      }
-}
