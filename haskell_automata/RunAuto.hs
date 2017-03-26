import Auto
import Data.Maybe
import System.Environment
import System.IO
import System.IO.Error
import Text.Read

-- Konstruuje monadę z błędem
errorText :: String -> String -> IO a
errorText reason line = ioError (userError $ reason ++ ": " ++ line)

-- Pomija puste linie
skipEmpty :: Handle -> IO String
skipEmpty h = do
  line <- hGetLine h
  if words line /= [] then return line
     else skipEmpty h

-- Wczytuje liczbę stanów ze stringa
stateNumber :: String -> IO Int
stateNumber s = case words s of
  [x] -> case readMaybe s of
    Just x | x < 0 -> errorText "Negative state number" s
    Just x | x >= 0 -> return x
    Nothing -> errorText "Invalid state number" s
  _ -> errorText "Invalid line format " s

-- Wczytuje listę stanów
stateList :: String -> Int -> IO [Int]
stateList s max = 
  let
    wrong x = x < 1 || x > max
  in case words s of
    [x] -> case readMaybe x of
      Just xs | any wrong xs -> errorText "Invalid state number" s
      Just xs -> return xs
      Nothing -> errorText "State numbers not parsed" s
    _ -> errorText "Invalid line format" s

-- Wczytuje przejścia automatu
transitionList :: Handle -> Int -> IO (String, [(Int, Char, [Int])])
transitionList h max = 
  let
    -- Parsuje linię z stanami
    readTrans :: [Char] -> [Char] -> [[Char]] -> String -> IO [(Int, Char, [Int])]
    readTrans st sym to line = 
      let
        -- Parsowanie liczb
        begin = readMaybe st
        toStates = map readMaybe to
        -- Weryfikacja poprawności
        validStates = all isJust (begin:toStates)
        validChar c = 'A' <= c && c <= 'Z'
        validSym = all validChar sym
        isValidMaybe n = case n of
          Just x -> 1 <= x && x <= max
          Nothing -> False
        validNum = all isValidMaybe (begin:toStates)
        -- Mapowanie na inty
        leaveMaybe n = case n of
          Just x -> x
          Nothing -> error "Internal code correctness error"
        op e xs = (leaveMaybe begin, e, map leaveMaybe toStates):xs
      in if not validStates then errorText "Invalid state type" line
      else if not validSym then errorText "Invalid label symbol" line
      else if not validNum then errorText "State number out of range" line
      else return $ foldr op [] sym
    
    -- Czyta i parsuje kolejne linie wejścia
    go :: Handle -> IO String -> [(Int, Char, [Int])] -> IO (String, [(Int, Char, [Int])])
    go h line acc = do
      line <- tryIOError line
      case line of
        Left e -> errorText "File unexpectedly ended" "EOF"
        -- Linia została wczytana, teraz parsing
        Right s -> case words s of
          -- Linia składa się z co najmniej 3 słów
          st:sym:w:ws -> do
            tran <- readTrans st sym (w:ws) s
            go h (skipEmpty h) (acc++tran)
          -- Linia składa się z dokładnie 1 słowa
          -- Zatem być może jest to słowo na którym ma działać automat
          [sym] -> return (sym, acc)
          -- Linia ma niepoprawny format
          _ -> errorText "Invalid transition format" s
  in go h (skipEmpty h) []

-- Sprawdzenie śmieci na końcu pliku
checkGarbage :: Handle -> IO ()
checkGarbage h = do
  line <- tryIOError $ skipEmpty h
  case line of
    Left _ -> return ()
    Right l -> errorText "Garbage at the end of file" l

-- Sparsowanie słowa wejściowego i wykonanie automatu
testAutomata :: String -> [Int] -> [Int] -> [Int] -> [(Int, Char, [Int])] -> IO String
testAutomata word st init acc tr = do
  let auto = fromLists st init acc tr
  let validChar c = 'A' <= c && c <= 'Z'
  let correct = all validChar word
  if not correct then errorText "Tested word not in alphabet" word
  else if accepts auto word then return "True"
  else return "False"

-- Parsuje i wykonuje automat
parseFile :: Handle -> IO String
parseFile h = do
  -- Pobieramy liczbę stanów
  line <- skipEmpty h
  states <- stateNumber line
  
  -- Stany początkowe
  line <- skipEmpty h
  starting <- stateList line states
  
  -- Stany akceptujące
  line <- skipEmpty h
  accepting <- stateList line states
  
  -- Przejścia i słowo do wykonania na automacie
  (word, transitions) <- transitionList h states
  
  -- Sprawdzenie, czy na końcu pliku nie ma śmieci
  checkGarbage h

  -- Wykonanie automatu
  result <- testAutomata word [1..states] starting accepting transitions
  
  hClose h -- Niebezpieczna sprawa
  return result

-- Otwiera plik do odczytu i woła parsowanie
tryFile :: String -> IO ()
tryFile file = do
  inh <- tryIOError $ openFile file ReadMode
  case inh of
    Left _ -> putStrLn "BAD INPUT\nInvalid file name"
    Right h -> do
      let err e = return $ "BAD INPUT\n"++(show e)
      res <- catchIOError (parseFile h) err
      putStrLn res

-- Funkcja main
main = do
  args <- getArgs
  case args of
    (file:_) -> tryFile file
    _ -> putStrLn "BAD INPUT\nFile name not specified"
