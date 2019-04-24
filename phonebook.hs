import Data.List

data Entry = Entry { name :: String
                   , phone :: String
                   } deriving(Show)

data Phonebook = Phonebook { entries :: [Entry] } deriving(Show)

instance Eq Entry where
  a == b = (name a == name b) && (phone a == phone b)

data State = State Int | InitState

addEntry :: Phonebook -> String -> String -> Phonebook
addEntry phonebook name phone =
 let entry = Entry{name = name, phone = phone}
     currentEntries = entries phonebook
     resultEntries = currentEntries ++ [entry]
  in phonebook{ entries = resultEntries }

findEntry :: Phonebook -> String -> Maybe Entry
findEntry phonebook value =
  find (\a -> name a == value) (entries $ phonebook)

deleteEntry :: Phonebook -> String -> Phonebook
deleteEntry phonebook n =
  case (findEntry phonebook n) of
    Just entry ->
      let currentEntries = entries phonebook
          resultEntries = delete entry currentEntries
      in phonebook { entries = resultEntries }
    Nothing -> phonebook

command :: Phonebook -> State -> IO()
command phonebook InitState = do
  let lines = ["Please, select a command",
               "0. Help",
               "1. Add new Entry",
               "2. Delete Entry",
               "3. Show Phonebook"]
  mapM_ putStrLn lines
  input <- getLine
  command phonebook (State $ read $ input)

command phonebook (State 1) = do
  putStrLn "Fill name:"
  name <- getLine
  putStrLn "Fill phone:"
  phone <- getLine
  putStrLn "Entry has been successfully added"
  command (addEntry phonebook name phone) InitState

command phonebook (State 2) = do
  putStrLn "Fill name:"
  name <- getLine
  putStrLn "Entry has been successfully deleted"
  command (deleteEntry phonebook name) InitState

command phonebook (State 3) = do
  putStrLn $ show $ phonebook
  command phonebook InitState

main =
  let me = Entry{name = "Kolqa", phone = "+712345" }
      phonebook = Phonebook [me]
  in command phonebook InitState
