import System.Environment

printer :: [String] -> IO ()
printer [] = return ()
printer (l:ls) = do
  putStrLn l
  printer ls

main :: IO ()
main = do
  args <- getArgs
  printer args

-- Lepsza wersja
-- main = getArgs >>= mapM_ putStrLn
