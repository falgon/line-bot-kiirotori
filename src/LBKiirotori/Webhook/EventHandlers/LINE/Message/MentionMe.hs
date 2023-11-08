module LBKiirotori.Webhook.EventHandlers.LINE.Message.MentionMe (
    repliedMe
) where

import           Control.Arrow                                  ((|||))
import           Control.Exception.Safe                         (throwString)
import           Control.Monad                                  (void)
import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Maybe                      (MaybeT (..))
import           Data.Functor                                   ((<&>))
import qualified Data.Text                                      as T
import           Data.Void                                      (Void)
import qualified Text.Megaparsec                                as M
import qualified Text.Megaparsec.Char                           as MC

import           LBKiirotori.Internal.Utils                     (hoistMaybe)
import           LBKiirotori.Webhook.EventObject.EventMessage   (LineEventMessage (..))
import           LBKiirotori.Webhook.EventObject.LineBotHandler

-- <mention me> ::= (<space>*) "@kiirotori" <space>
mentionMeP :: Ord e
    => M.ParsecT e T.Text (MaybeT LineBotHandler) ()
mentionMeP = MC.space
    *> (lift (lift askLineChanName) >>= void . MC.string . (T.singleton '@' <>))
    <* MC.space1

-- <replied message> ::= <mention me> <cmd>
repliedMeParser :: M.ParsecT Void T.Text (MaybeT LineBotHandler) (Maybe T.Text)
repliedMeParser = M.option Nothing $ M.try (mentionMeP *> M.getInput <&> Just)

repliedMe :: LineEventMessage
    -> LineBotHandler (Maybe T.Text)
repliedMe mobj = runMaybeT $
    hoistMaybe (lemText mobj)
        >>= M.runParserT repliedMeParser mempty
        >>= (lift . throwString . M.errorBundlePretty ||| hoistMaybe)
