import System.Environment
import System.IO

zliczarka :: Handle -> Int -> Int -> Int -> IO String
zliczarka h linie slowa znaki = do
  eof <- hIsEOF h
  if eof then do
    return $ (show linie)++" "++(show slowa)++" "++(show znaki)
  else do
    line <- hGetLine h
    let ln = linie + 1
    let sl = slowa + (length $ words line)
    let zn = znaki + (length line)
    zliczarka h ln sl zn

main :: IO ()
main = do
  [x] <- getArgs
  h <- openFile x ReadMode
  wynik <- zliczarka h 0 0 0
  putStrLn wynik
  hClose h
