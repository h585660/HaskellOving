module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)

eval :: (Ord variable) => Expr variable -> Map variable Bool -> Maybe Bool
eval (Var name) values = Map.lookup name values
eval (Lit truthValue) _ = Just truthValue

eval (And leftExpr rightExpr) values = 
    case (eval leftExpr values, eval rightExpr values) of
        (Just True, Just True) -> Just True
        (Nothing, _)           -> Nothing
        (_, Nothing)           -> Nothing
        _                      -> Just False

eval (Or leftExpr rightExpr) values = 
  case (eval leftExpr values, eval rightExpr values) of
      (Just True, Just _)    -> Just True
      (Just _, Just True)    -> Just True
      (Nothing, _)           -> Nothing
      (_, Nothing)           -> Nothing
      _                      -> Just False

