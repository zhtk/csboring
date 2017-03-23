import Control.Monad.Error.Class
import Data.Char

data ParseError = Err {location::Int, reason::String}

instance Error ParseError where
  noMsg = Err (-1) "unknown"
  strMsg msg = Err (-1) msg

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit z loc
 | isHexDigit z = return $ toInteger $ digitToInt z
 | otherwise = throwError $ Err loc ("can't parse digit "++[z])

parseHex :: String -> ParseMonad Integer
parseHex [] = throwError $ Err 0 "string is empty"
parseHex xs = go xs 1 0 where
  go [] _ acc = return acc
  go (d:ds) l acc = do 
    cyfra <- parseHexDigit d l
    res <- go ds (l+1) (acc*16 + cyfra)
    return res

toString :: Integer -> ParseMonad String
toString x = return $ show x

-- convert zamienia napis z liczba szesnastkowa 
-- na napis z liczba dziesietna
convert :: String -> String
convert s = str where
  (Right str) = tryParse s `catchError` printError
  tryParse s = do {n <- parseHex s; toString n}
  printError e = return $ "Error: "++(reason e)++" at position "++(show $ location e)
