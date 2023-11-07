module LBKiirotori.Webhook.EventHandlers.LINE.Message.Parser (
    lexeme
) where

import           Control.Applicative        (Alternative (..))
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

spaceConsumer :: Ord e
    => M.ParsecT e T.Text m ()
spaceConsumer = MCL.space MC.space1 empty empty

lexeme :: Ord e
    => M.ParsecT e T.Text m a
    -> M.ParsecT e T.Text m a
lexeme = MCL.lexeme spaceConsumer

