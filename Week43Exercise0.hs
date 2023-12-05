module Week43Exercise0 where


data BinSearchTree a
  = Leaf a
  | LeftRightChildBranch (BinSearchTree a) a (BinSearchTree a)
  | LeftChildBranch (BinSearchTree a) a
  | RightChildBranch a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where
  foldr func res (Leaf val) = func val res
  foldr func res (LeftChildBranch left val) = foldr func (func val res) left
  foldr func res (RightChildBranch val right) = func val (foldr func res right)
  foldr func res (LeftRightChildBranch left val right) = foldr func (func val (foldr func res right)) left
  
toList :: BinSearchTree a -> [a]
toList = foldr (:) []
