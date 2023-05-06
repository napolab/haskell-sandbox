import Data.Monoid
import Control.Monad.Writer
import System.Random

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList ([] ++)

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcd'' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell [show x]

add :: Num a => a -> a -> a
add x y = x + y

multi :: Num a => a -> a -> a
multi x y = x * y

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a + b)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in  (firstCoin, secondCoin, thirdCoin)

main = do
  print 1
  print (("beans", Sum 10) `applyLog` addDrink)
  print (("bean", Sum 10) `applyLog` addDrink)
  print (runWriter (return 3 :: Writer String Int))
  print (runWriter (return "test" :: Writer (Sum Int) String))
  let r = runWriter (return 3 :: Writer (Product Int) Int)
  print r
  print $ runWriter multWithLog
  print $ runWriter (gcd' 2 0)
  print $ fst $ runWriter $ gcd' 8 3

  mapM_ putStrLn . snd . runWriter $ gcdReverse 110 34
  mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd'' 110 34

  let a = add <$> multi 2
  print $ a 2 10
  let b = multi 10
  print $ b 2
  let c = multi <*> multi 10
  -- <*> :: m (a -> b) ->  ma -> mb
  -- let f = (a -> a -> a) <*> (a -> a)
  -- :t f
  -- f :: a -> a
  print $ c 2

  let f = add <$> multi 2 <*> add 10
  print $ f 1
  print $ addStuff 1
