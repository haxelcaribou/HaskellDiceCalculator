module Tree (Tree (Leaf, Branch)) where

data Tree a = Leaf a | Branch a [Tree a] deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x l) = Branch (f x) $ map (fmap f) l