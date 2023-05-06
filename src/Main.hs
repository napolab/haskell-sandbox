import qualified Data.Foldable as F
import Data.Monoid
import Text.XHtml (base)
import Language.Haskell.TH (Role)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r

testTree :: Tree Integer
testTree = Node 5
            (
              Node 3
              (Node 1 EmptyTree EmptyTree)
              (Node 6 EmptyTree EmptyTree)
            )
            (
              Node 9
              (Node 8 EmptyTree EmptyTree)
              (Node 10 EmptyTree EmptyTree)
            )

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

pole :: Maybe Pole
pole = do
  landLeft' 1 (0, 0) >>= landRight' 4 >>= landLeft' (-1) >>= landRight' (-2)

routine :: Maybe Pole
routine = case landLeft' 1 (0, 0) of
  Nothing -> Nothing
  Just pole1 -> case landRight' 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft' 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft' 1 pole3

main :: IO ()
main = do
  print (F.foldl (+) 0 testTree)
  print (F.foldl (-) 0 testTree)
  print (F.foldl (*) 2 (Just 9))
  print (F.foldl (||) False (Just True))
  print (F.foldMap (\x -> Any $ x > 15) testTree)
  print (F.foldMap (: []) testTree)
  let Just x = Just (+3) <*> Just 3
  print (Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing)
  print (landLeft 2 (0, 0))
  print (landRight 2 (1, 2))
  print pole

  case routine of
    Nothing -> print "nothing"
    Just (x, y) -> print (x, y)