module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)


toBinarySearchTree :: [a] -> BinSearchTree a
toBinarySearchTree [] = Empty
toBinarySearchTree xs = Branch (toBinarySearchTree left) root (toBinarySearchTree right)
  where
    mid =length xs `div` 2
    root = head (drop mid xs)
    left = take mid xs 
    right = tail (drop mid xs)