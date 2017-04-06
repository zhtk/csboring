module StateTParser(Parser,runParser,item) where
import Control.Monad.State

-- Use the StateT transformer on Maybe
type Parser a = StateT [Char] Maybe a

runParser = runStateT
-- instance Monad ... gratis!
-- instance MonadPlus ... gratis!
-- instance MonadState ...gratis!

item :: Parser Char
--item :: (MonadPlus m) => StateT [a] m a
item = do
  input <- get
  case input of
    [] -> mzero
    (x:xs) -> put xs >> return x
