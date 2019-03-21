-- import HUnit
import Data.Function

sieve :: Int -> [Int]
sieve n = sieve' [2..n]
-- [2..n]

-- test1 = TestCase (assertEqual(sieve(5) [2, 3, 5]))
-- tests = TestList [TestLabel "Main test" test1]

sieve' :: [Int] -> [Int]
sieve' [] = []
sieve' (head:tail) = head : sieve' (sieve'' head tail)

sieve'' :: Int -> [Int] -> [Int]
sieve'' _ [] = []
sieve'' current (head:tail) = do
  case mod head current of
    0 -> sieve'' current tail
    _ -> head : sieve'' current tail

main = do
  putStrLn "n="
  n <- getLine
  putStrLn $ show $ sieve $ read n
  -- read n & sieve & show & putStrLn
  -- putStrLn (show (sieve (read n)))
