f (str, num) znak
  | num == 30 = (str++['\n',znak], 1)
  | otherwise = (str++[znak], num + 1)

smain :: String -> String
smain str =
  fst $ foldl f ("", 1) str
main = interact smain
