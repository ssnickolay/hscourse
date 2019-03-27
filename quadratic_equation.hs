import Data.Function
import Data.Maybe

discriminant :: Int -> Int -> Int -> Maybe Int
discriminant a b c = do
  let d = b * b - 4 * a * c
  if d < 0
    then Nothing
    else Just(d)

div' = (/) `on` fromIntegral

getX' :: Int -> Int -> Int -> [Maybe Float]
getX' a b 0 =
  let top = -1 * b
      bottom = 2 * a
      in [Just $ top `div'` bottom]
getX' a b d =
  -- let v = sqrt (fromIntegral d)
  let v = sqrt 0.2
      top = \sign -> -1 * b + sign * v
      bottom = 2 * a
      in [Just 1.0]
      -- in [Just $ top 1 `div'` bottom]
  --   Just $(-1 * b + sqrt(d)) `div'` (2 * a)),
  --   Just ((-1 * b - sqrt(d)) `div'` (2 * a))
  -- ]

-- calculate :: Int -> Int -> Int -> [Maybe Int]
-- calculate a b c = do
--   -- d <- fmap (+ 1) $ discriminant a b c
--   d <- fmap (+ 1) $ 1
--   getX' a b d

main = do
  putStrLn "a="
  a <- getLine
  putStrLn "b="
  b <- getLine
  putStrLn "c="
  c <- getLine

  putStrLn "A"
  -- calculate read a read b
  -- putStrLn $ show $ calculate (read a) (read b) (read c)
