import Data.Char

readInts :: String -> [Int]
readInts str = 
  let
    ws = filter (all isDigit) $ words str
  in map read ws

readInts2 :: String -> Either String [Int]
readInts2 str =
  let
    errs = filter (not . all isDigit) $ words str
  in case errs of
    (e:_) -> Left ("Not a number: "++e)
    [] -> Right (readInts str)

sumInts :: String -> String
sumInts str = 
  case readInts2 str of
    Left e -> e
    Right vals -> show $ foldl (+) 0 vals

main = interact sumInts
