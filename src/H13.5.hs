foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Nothing
  y <- Just "!"
  Just "1"

type Birds = Int
type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing



routine :: Maybe Pole
routine = do
  let start = (0, 0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  landLeft' 1 second

main = do
  print (Just 1 >>= (\x -> Just (show x ++ "!")))
  print (Just "a" >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
  print (let x = 3; y = "!" in show x ++ y)
  print foo
  print foo'
  print routine
