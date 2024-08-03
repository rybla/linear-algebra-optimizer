module Types where

import Data.List (List)
import Data.Maybe (Maybe)

-- =============================================================================
-- Math

type S = Number
type V = Array S
type M = Array V

-- =============================================================================
-- Tree

data Tree a = Tree a (List (Tree a))

-- =============================================================================
-- Expr

type Expr = Tree ExprLabel

data ExprLabel
  = Lit M -- m*n -> m*n
  | Add -- m*n, m*n -> m*n
  | Scale -- 1*1, m*n -> m*n
  | Dot -- m*n, n*m -> m*m
  | Cross -- m*n, m*n -> m*n
  | Transpose -- m*n -> n*m
  | Inverse -- n*n -> n*n
  | Determinant -- n*n -> 1*1

-- =============================================================================
-- Rule

newtype Rule = Rule (Expr -> Maybe Expr)

