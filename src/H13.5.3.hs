justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x
wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

errorH :: Char
errorH = x where (x:xs) = ""

main = do
  print justH
  print wopwop