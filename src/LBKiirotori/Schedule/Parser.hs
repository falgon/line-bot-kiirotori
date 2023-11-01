{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module LBKiirotori.Schedule.Parser (
    SchedulableAppCmd (..)
  , SchedulableApp (..)
  , TargetSchedule (..)
  , ScheduleEntry (..)
  , parseSchedule
) where

import           Control.Applicative        (Alternative (..))
import           Control.Arrow              ((|||))
import           Control.Exception.Safe     (MonadThrow (..), throwString)
import           Control.Monad              (forM, void, (>=>))
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (Reader (..), asks, runReader)
import           Control.Monad.Trans.State  (StateT, evalStateT, gets, modify)
import           Data.Functor               (($>), (<&>))
import           Data.Functor.Identity      (Identity)
import           Data.List                  (isPrefixOf)
import qualified Data.Map                   as Map
import           Data.Char                  (isSpace)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified System.Cron.Parser         as C
import qualified System.Cron.Types          as C
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as MCL
import           Text.Printf                (printf)

import           LBKiirotori.Config         (LBKiirotoriConfig)
import           LBKiirotori.Internal.Utils (fromMaybeM)

type Variables = Map.Map T.Text T.Text
type EntryParser = M.ParsecT Void T.Text (Reader Variables)

lexeme :: Ord e
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m a
lexeme = MCL.lexeme spaceConsumer
    where
        spaceConsumer = MCL.space MC.hspace1 (MCL.skipLineComment "#") empty

separator :: Ord e
    => M.ParsecT e T.Text m ()
separator = M.skipSome MC.space1 M.<|> void (M.skipSome MC.tab)

-- <variable> ::= "$" [a-zA-Z_]{1,}[a-zA-Z0-9_]{0,}
variable :: EntryParser T.Text
variable = do
    vname <- MC.char '$'
        *> M.choice [
            M.between (MC.char '{') (MC.char '}') varValid
          , varValid
          ]
    lift (asks $ Map.lookup vname)
        >>= fromMaybeM (fail $ printf "undefined variable '$%s'" $ T.unpack vname)
    where
        varHead = M.choice [ M.oneOf [ 'a' .. 'z' ], M.oneOf [ 'A' .. 'Z' ], MC.char '_' ]
        varTail = M.many (varHead M.<|> MC.digitChar M.<?> "[a-zA-Z0-9_]{0,}")
        varValid = T.cons <$> varHead <*> (T.pack <$> varTail)

expandVariable :: EntryParser a -> EntryParser a
expandVariable p = do
    val <- variable
    src <- M.getInput <* M.setInput val
    p <* M.setInput src

withVar :: EntryParser a -> EntryParser a
withVar p = M.choice [
    expandVariable p
  , p
  ]

-- <target id> ::= <id> <separator>
--
-- <id> ::= <user id>
--      | <group id>
--      | <room id>
--
-- <user id>    ::= U[0-9a-f]{32}
--
-- <group id>   ::= C[0-9a-f]{32}
--
-- <room id>    ::= R[0-9a-f]{32}
--
-- c.f. https://developers.line.biz/ja/faq/#what-are-userid-groupid-and-roomid
targetId :: Ord e => M.ParsecT e T.Text m T.Text
targetId = T.cons
    <$> M.choice [ MC.char 'U', MC.char 'C', MC.char 'R' ]
    <*> (T.pack <$> M.count 32 (MC.digitChar M.<|> M.oneOf ['a'..'z']))

targetIdWithVar :: EntryParser T.Text
targetIdWithVar = withVar targetId

data SchedulableAppCmd = PushTextMessage
    deriving (Show, Eq)

instance Read SchedulableAppCmd where
    readsPrec _ s
        | "push-text-message" `isPrefixOf` s = [(PushTextMessage, drop 17 s)]
        | otherwise = []

schedulableApp :: Ord e => M.ParsecT e T.Text m SchedulableAppCmd
schedulableApp = M.choice
    [ read . T.unpack <$> MC.string "push-text-message"
    ]

schedulableAppWithVar :: EntryParser SchedulableAppCmd
schedulableAppWithVar = withVar schedulableApp

-- FIXME: Setting a variable to a value with spaces will result in a parse error.
-- This is a specification of `System.Cron.Parser.parseCrontab`.
schedulableAppArg :: EntryParser [T.Text]
schedulableAppArg = M.sepBy (withVar (quoted M.<|> nonQuoted)) separator
    where
        singleQuoted = T.pack <$> (MC.char '\'' *> M.someTill MCL.charLiteral (MC.char '\''))
        doubleQuoted = T.pack <$> (MC.char '"' *> M.someTill MCL.charLiteral (MC.char '"'))
        quoted = lexeme (singleQuoted M.<|> doubleQuoted)
        nonQuoted = T.pack <$> M.some (M.satisfy (not . isSpace))

schedulableAppArgWithVar :: EntryParser [T.Text]
schedulableAppArgWithVar = M.option [] (lexeme separator *> schedulableAppArg)

data SchedulableApp = SchedulableApp {
    saCmd :: SchedulableAppCmd
  , saArg :: [T.Text]
  }
  deriving (Show, Eq)

data TargetSchedule = TargetSchedule {
    saTargetId :: T.Text
  , saApp      :: SchedulableApp
  }
  deriving (Show, Eq)

data ScheduleEntry = ScheduleEntry {
    seSchedule       :: C.CronSchedule
  , seTargetSchedule :: TargetSchedule
  }
  deriving (Show, Eq)

runEntryParser :: MonadThrow m
    => EntryParser a
    -> T.Text
    -> Variables
    -> m a
runEntryParser p body = ((throwString . M.errorBundlePretty) ||| pure)
    . runReader (M.runParserT p mempty body)

parseCrontab :: MonadThrow m
    => T.Text
    -> m [C.CrontabEntry]
parseCrontab = (throwString ||| (pure . C.crontabEntries))
    . C.parseCrontab

runVariableState :: Monad m
    => StateT Variables m a
    -> m a
runVariableState = flip evalStateT Map.empty

entry :: EntryParser TargetSchedule
entry = TargetSchedule
    <$> lexeme (targetIdWithVar <* separator)
    <*> (SchedulableApp
        <$> schedulableAppWithVar
        <*> schedulableAppArgWithVar)

insertWithOverWrite :: Ord k
    => k
    -> a
    -> Map.Map k a
    -> Map.Map k a
insertWithOverWrite key val m
    | Map.member key m = Map.update (const $ Just val) key m
    | otherwise = Map.insert key val m

parseSchedule :: MonadThrow m
    => T.Text
    -> m [ScheduleEntry]
parseSchedule = parseCrontab
    >=> runVariableState . variableState
    >=> pure . catMaybes
    where
        variableState pt = forM pt $ \case
            C.EnvVariable name val -> modify (insertWithOverWrite name val)
                $> Nothing
            C.CommandEntry schedule cmd -> gets (runEntryParser entry (C.cronCommand cmd))
                >>= lift
                <&> Just . ScheduleEntry schedule
