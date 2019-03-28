import Data.Function
import Data.Maybe

discriminant :: Float -> Float -> Float -> Maybe Float
discriminant a b c = do
  let d = b * b - 4 * a * c
  if d < 0
    then Nothing
    else Just(d)

-- div' = (/) `on` fromIntegral

roots :: Float -> Float -> Float -> Maybe [Float]
roots a b 0 = -- one root
  let top = -1 * b
      bottom = 2 * a
      in Just $ [top / bottom]

roots a b d = -- two roots
  let top = \sign -> (-1 * b) + (sign) * sqrt d
      bottom = 2 * a
      x1 = (top 1) / bottom
      x2 = (top (-1.0)) / bottom
      in Just $ [x1, x2]

calculate :: Float -> Float -> Float -> Maybe [Float]
calculate a b c =
  -- d <- discriminant a b c
  -- roots a b d
  discriminant a b c >>= roots a b

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
