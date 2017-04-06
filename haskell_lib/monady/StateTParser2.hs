module StateTParser2(Parser,runParser,item) where
import Control.Monad.State
import MaybeTrans
import Control.Monad.Identity

type Parser a = StateT [Char] (MaybeT Identity) a

runParser :: Parser a -> [Char] -> Maybe (a,String)
runParser p x1 = runIdentity $ runMaybeT $ runStateT p x1

item :: Parser Char
--item :: (MonadPlus m) => StateT [a] m a
item = do
  input <- get
  case input of
    [] -> mzero
    (x:xs) -> put xs >> return x

