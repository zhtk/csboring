import Text.Read
import Control.Monad.Error
--import Control.Monad.Except

czytaj :: String -> Either String Int
czytaj s = case readMaybe s of
  Just x -> return x
  Nothing -> throwError $ "Error: unparsable "++s

readInts2 :: String -> Either String [Int]
readInts2 s = 
  let list = words s in
  if length list == 0 then return []
  else do
    num <- czytaj $ head list 
    reszta <- readInts2 $ unwords $ tail list
    return $ num:reszta

sumInts :: String -> String
sumInts s = 
  case readInts2 s of
    Left e -> e
    Right xs -> show $ foldl (+) 0 xs
