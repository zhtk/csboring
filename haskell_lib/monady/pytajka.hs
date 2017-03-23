main :: IO ()
main = do
  putStrLn "Podaj ulubiony język programownia:"
  line <- getLine
  if line == "haskell" then return ()
  else main
