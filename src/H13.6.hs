import Control.Monad (guard)

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

filterElem :: (a -> Bool) -> [a] -> [a]
filterElem f xs = do
  x <- xs
  guard (f x)
  return x

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

main :: IO ()
main = do
  print listOfTuples
  print [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]
  print (guard (5 > 2) :: Maybe ())
  print (guard (1 < 2) :: [()])
  print ([1..50] >>= (\x -> guard ('7' `elem` show x) >> return x))
  print (filterElem (\x -> '7' `elem` show x) [1..100])
  print (guard (5 > 2) >> return "cool" :: [String])
  print (guard (1 > 2) >> return "cool" :: [String])
