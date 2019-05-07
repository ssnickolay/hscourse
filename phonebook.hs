import Data.List
import Test.QuickCheck

data Entry = Entry { name :: String
                   , phone :: String
                   } deriving(Show)

data Phonebook = Phonebook { entries :: [Entry] } deriving(Show)

instance Eq Entry where
  a == b = (name a == name b) && (phone a == phone b)

data Command =  RawCommand String | HelpCommand | ShowPhonebookCommand | AddEntryCommand String String | DeleteEntryCommand String | Quit
data Response = PrintHelp | PrintPhonebook
data State = InitState Phonebook | Wait Phonebook

addEntry :: Phonebook -> String -> String -> Phonebook
addEntry phonebook name phone =
 let entry = Entry{name = name, phone = phone}
     currentEntries = entries phonebook
     resultEntries = currentEntries ++ [entry]
  in phonebook{ entries = resultEntries }

instance Arbitrary Entry where
  arbitrary = do
    name <- arbitrary
    phone <- arbitrary
    return $ Entry{name = name, phone = phone}

instance Arbitrary Phonebook where
  arbitrary = do
    entry <- arbitrary
    return (Phonebook [entry])

prop_AddEntry phonebook name phone =
    let Phonebook{ entries = entries } = addEntry phonebook name phone
        (old:Entry { name = n, phone = p }:[]) = entries
  in name == n && phone == p

findEntry :: Phonebook -> String -> Maybe Entry
findEntry phonebook value =
  find (\a -> name a == value) (entries $ phonebook)

prop_SuccessFindEntry phonebook =
  let [entry] = entries phonebook
      foundedEntry = findEntry phonebook (name entry)
  in foundedEntry == Just(entry)

prop_NothingFindEntry phonebook value =
  findEntry phonebook (value ++ "salt") == Nothing

deleteEntry :: Phonebook -> String -> Phonebook
deleteEntry phonebook n =
  case (findEntry phonebook n) of
    Just entry ->
      let currentEntries = entries phonebook
          resultEntries = delete entry currentEntries
      in phonebook { entries = resultEntries }
    Nothing -> phonebook

getCommand :: State -> IO Command
getCommand (InitState _) = do
  return HelpCommand

getCommand (Wait _) = do
  s <- getLine
  getCommand' $ RawCommand $ s

getCommand' :: Command -> IO Command
getCommand' (RawCommand "exit") = return Quit
getCommand' (RawCommand "0") = return HelpCommand
getCommand' (RawCommand "1") = do
  putStrLn "Fill name:"
  name <- getLine
  putStrLn "Fill phone:"
  phone <- getLine
  return $ AddEntryCommand name phone

getCommand' (RawCommand "2") = do
  putStrLn "Delete Entry with name:"
  name <- getLine
  return $ DeleteEntryCommand name

getCommand' (RawCommand "3") = return ShowPhonebookCommand

handleCommand :: State -> Command -> (State, Response)
handleCommand (InitState phonebook) HelpCommand =
  (Wait phonebook, PrintHelp)

handleCommand (Wait phonebook) HelpCommand =
  (Wait phonebook, PrintHelp)

handleCommand (Wait phonebook) ShowPhonebookCommand =
  (Wait phonebook, PrintPhonebook)

handleCommand (Wait phonebook) (AddEntryCommand name phone)=
  let newPhonebook = addEntry phonebook name phone
  in (Wait newPhonebook, PrintPhonebook)

handleCommand (Wait phonebook) (DeleteEntryCommand name)=
  let newPhonebook = deleteEntry phonebook name
  in (Wait newPhonebook, PrintPhonebook)

handleResponse :: State -> Response -> IO ()
handleResponse _ PrintHelp = do
  let lines = ["Please, select a command",
               "0. Help",
               "1. Add new Entry",
               "2. Delete Entry",
               "3. Show Phonebook"]
  mapM_ putStrLn lines

handleResponse (Wait phonebook) PrintPhonebook = do
  putStrLn "Current Phonebook:"
  putStrLn $ show $ phonebook

loop :: State -> IO()
loop state = do
  command <- getCommand state
  let (newState, response) = handleCommand state command
  handleResponse newState response
  loop newState

main :: IO ()
main =
  let me = Entry{name = "Kolqa", phone = "+712345" }
      phonebook = Phonebook [me]
  in loop $ InitState phonebook


test = do
  quickCheck prop_AddEntry
  quickCheck prop_SuccessFindEntry
  quickCheck prop_NothingFindEntry
