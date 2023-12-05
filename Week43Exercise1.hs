module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap func (Branch nodeValue childTrees) = Branch (func nodeValue) (map (fmap func) childTrees)

productNodes :: (Num a) => RoseTree [a] -> RoseTree a
productNodes = fmap multiplyList
  where 
    multiplyList numbersList = 
      if null numbersList 
      then 1 
      else foldl (\acc num -> acc * num) 1 numbersList

