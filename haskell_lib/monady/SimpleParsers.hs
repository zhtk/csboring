module SimpleParsers where
import Data.Char(isDigit,digitToInt)
import Control.Monad

-- Importujemy jeden z poniższych modułów:
-- import StateTParser2
import StateTParser
-- import StateParser
-- import StateErrorParser(Parser,runParser,item)
-- import DummyParser(Parser,runParser,item)

pDigit1 :: Parser Int
pDigit1 = item >>= test where
  test x | isDigit x = return $ digitToInt x
         | otherwise = mzero

sat :: (Char->Bool) -> Parser Char
sat p = do {x <- item; if p x then return x else mzero}

char :: Char -> Parser Char
char x = sat (==x)

-- Lepsza wersja pDigit1
pDigit :: Parser Int
pDigit = fmap digitToInt $ sat isDigit

pDigits :: Parser [Int]
pDigits = many pDigit

pNat :: Parser Int 
-- pNat = pDigits >>= return . foldl (\x y -> 10*x+y) 0
pNat = fmap (foldl (\x y -> 10*x+y) 0) pDigits

many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

many1 p = do { a <- p; as <- many p; return (a:as)}

test123 = runParser pNat "123 ala"
