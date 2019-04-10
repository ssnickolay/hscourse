class DuckBoolean a where
  true, false :: a -> Bool
  true a = not $ false a
  false a = not $ true a


newtype String2 = String2 String

data Nil = Nil
data DuckType = DuckTypeS String2 | DuckTypeI Int | DuckTypeN Nil

instance DuckBoolean String2 where
  true _ = True

instance DuckBoolean Int where
  true 0 = False
  true _ = True

instance DuckBoolean Nil where
  true _ = False

instance DuckBoolean DuckType where
  true (DuckTypeS s) =  true s
  true (DuckTypeI i) =  true i
  true (DuckTypeN n) =  true n

myIf :: DuckType -> a -> a -> a
myIf duck right left = if true duck then right else left

main = do
  putStrLn $ show $ (myIf (DuckTypeS $ String2 "foo") 1 0) -- 1
  putStrLn $ show $ (myIf (DuckTypeS $ String2 "") "Any String is True" "error")
  putStrLn $ show $ (myIf (DuckTypeI 0) "error" "0 is False")
  putStrLn $ show $ (myIf (DuckTypeI 100) "100 is True" "error")
  putStrLn $ show $ (myIf (DuckTypeN Nil) "error" "Nil is False")
