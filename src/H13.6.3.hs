import Control.Monad
type KnightPos = (Int, Int)

foo (c, r) = do
  (x, y) <- [(x', y') | x' <- [-2..2], y' <- [-2..2]]
  (c', r') <- [(c + x, r + y)]

  return (c', r')


moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- foo (c, r)
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

main = do
  let pos :: KnightPos  = (6, 2)
  print ((+) <$> [1, 2] <*> [6, 2])
  print ((-) <$> [1, 2] <*> [6, 2])

  print (moveKnight pos)
  print (pos `canReachIn3` (6, 1))
  print (pos `canReachIn3` (7, 3))