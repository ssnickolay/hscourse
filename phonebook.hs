data Entry = Entry { name :: String
                   , phone :: String
                   } deriving(Show)

data Phonebook = Phonebook { entries :: [Entry] } deriving(Show)
data State = State Int

command :: Phonebook -> State -> IO()
command phonebook state = do
  case state of
    State 0 -> do
      putStrLn "Please, select a command"
      putStrLn "0. Help"
      putStrLn "1. Add new Entry"
    _ ->
      putStrLn "Unknown command, try again"
  input <- getLine
  command phonebook (State $ read $ input)

main =
  let me = Entry{name = "Kolqa", phone = "+712345" }
      phonebook = Phonebook [me]
      initState = State 0
  in command phonebook initState
