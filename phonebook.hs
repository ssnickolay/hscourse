data Entry = Entry { name :: String
                   , phone :: String
                   } deriving(Show)

data Phonebook = Phonebook { entries :: [Entry] } deriving(Show)
data State = State Int | InitState

command :: Phonebook -> State -> IO()
command phonebook InitState = do
  let lines = ["Please, select a command",
               "0. Help",
               "1. Add new Entry",
               "2. Show Phonebook"]
  mapM_ putStrLn lines
  input <- getLine
  command phonebook (State $ read $ input)

command phonebook (State 1) = do
  putStrLn "Fill name:"
  name <- getLine
  putStrLn "Fill phone:"
  phone <- getLine
  putStrLn "Enty has been successfully added"
  let entry = Entry{name = name, phone = phone}
      currentEntries = entries phonebook
      resultEntries = currentEntries ++ [entry]
  command Phonebook{ entries = resultEntries } InitState

command phonebook (State 2) = do
  putStrLn $ show $ phonebook
  command phonebook InitState

main =
  let me = Entry{name = "Kolqa", phone = "+712345" }
      phonebook = Phonebook [me]
  in command phonebook InitState
