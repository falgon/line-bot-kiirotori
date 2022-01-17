{-# LANGUAGE OverloadedStrings #-}
module LBKiirotori.Schedule.Parser (
    SchedulableAppRow (..)
  , SchedulableAppCmd (..)
  , SchedulableApp (..)
  , parseCronSchedule
) where

import           Control.Applicative        (Alternative (..))
import           Control.Arrow              ((|||))
import           Control.Exception.Safe     (MonadThrow (..), throw)
import           Control.Monad              (void)
import           Data.Functor               (($>), (<&>))
import           Data.Functor.Identity      (Identity)
import           Data.List                  (isPrefixOf)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified System.Cron.Schedule       as C
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

import           LBKiirotori.Config         (LBKiirotoriConfig)

-- <space> ::= Unicode space character, and the control characters:
-- tab, newline, carriage return, form feed, and vertical tab
spaceConsumer :: Ord e
    => M.ParsecT e T.Text m ()
spaceConsumer = MCL.space MC.hspace1 (MCL.skipLineComment "#") empty

lexeme :: Ord e
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m a
lexeme = MCL.lexeme spaceConsumer

-- <separator> ::= ("\t")+ | (" ")+
separator :: Ord e
    => M.ParsecT e T.Text m ()
separator = M.skipSome MC.space1 M.<|> void (M.skipSome MC.tab)

-- <cron expr> ::= cron expression, see crontab(5) <separator>
cronExpr :: Ord e => M.ParsecT e T.Text m T.Text
cronExpr = T.intercalate " "
    <$> M.count 5 (T.pack <$> M.someTill M.anySingle separator)

-- <target id>  ::= <id> <separator>
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
    <* separator

data SchedulableAppCmd = PushMessage
    deriving (Show, Eq)

instance Read SchedulableAppCmd where
    readsPrec _ s
        | "push-message" `isPrefixOf` s = [(PushMessage, drop 12 s)]
        | otherwise = []

data SchedulableApp = SchedulableApp {
    saCmd :: SchedulableAppCmd
  , saArg :: [T.Text]
  }
  deriving (Show, Eq)

data SchedulableAppRow = SchedulableAppRow {
    sarCronExpr :: T.Text
  , sarTargetId :: T.Text
  , sarApp      :: SchedulableApp
  }
  deriving (Show, Eq)

-- <app cmd> ::= <cmd> <arguments>
--
-- <cmd> ::= "push-message"
--
-- <arguments> ::= ""
--      | <separator> <string> (<arguments>)*
appCmd :: Ord e => M.ParsecT e T.Text m SchedulableApp
appCmd = SchedulableApp
    <$> M.choice [
            read . T.unpack <$> MC.string "push-message"
          ]
    <*> arguments
    where
        arguments = M.choice [
            separator
                *> M.manyTill M.anySingle (M.choice [
                    M.eof
                  , MCL.skipLineComment "#"
                  , void $ M.lookAhead MC.newline
                  ])
                <&> T.words . T.pack
          , spaceConsumer
                *> (void (M.lookAhead MC.newline) M.<|> M.eof)
                $> []
          ]

-- <row> ::= <eof>
--      | <cron expr> <target id> <app cmd>
row :: M.ParsecT Void T.Text Identity SchedulableAppRow
row = SchedulableAppRow
    <$> cronExpr
    <*> targetId
    <*> appCmd

cronSchedule :: M.ParsecT Void T.Text Identity [SchedulableAppRow]
cronSchedule = spaceConsumer
    *> M.skipMany MC.newline
    *> M.sepEndBy (lexeme row) (M.skipMany MC.newline M.<|> M.eof)

parseCronSchedule :: MonadThrow m => T.Text -> m [SchedulableAppRow]
parseCronSchedule = (throw ||| pure) . M.parse cronSchedule mempty

-- parseSchedule :: T.Text -> C.ScheduleT m a
--
