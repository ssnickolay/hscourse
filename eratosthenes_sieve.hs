sieve :: Int -> [Int]
sieve n = sieve' (enumFromTo 2 n)

sieve' :: [Int] -> [Int]
sieve' [] = []
sieve' (head:tail) = [head] ++ sieve'(sieve'' head tail)

sieve'' :: Int -> [Int] -> [Int]
sieve'' _ [] = []
sieve'' current (head:tail) = do
  case mod head current of
    0 -> sieve'' current tail
    _ -> [head] ++ sieve'' current tail

main = do
  putStrLn "n="
  n <- getLine
  putStrLn (show (sieve (read n)))
