module Types where

import Prelude

import Data.Array as Array
import Data.Array.ST as Array.ST
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe')
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (unreachable)

-- =============================================================================
-- Meta

type Weight = Number

-- =============================================================================
-- Math

type S = Number
type V = Array S
type M = Array V

-- =============================================================================
-- Tree

data Tree a = Tree a (Array (Tree a))

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x = genericEq x

derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

unfoldTreeWithWrap :: forall a. Tree a -> Array ((Tree a -> Tree a) /\ Tree a)
unfoldTreeWithWrap t0 = Array.ST.run do
  results <- Array.ST.new
  let
    go wrap t@(Tree l ts) = do
      results # Array.ST.push (wrap /\ t) # void
      ts # traverseWithIndex_ \i -> go
        ( wrap <<<
            \tKid' -> Tree l (ts # Array.updateAt i tKid' # fromMaybe' (\_ -> unreachable "tree kid index out of bounds"))
        )
      pure unit
  go identity t0
  pure results

-- =============================================================================
-- Expr

type Expr = Tree ExprLabel

data ExprLabel
  = Matrix M -- m*n -> m*n
  | Scalar S
  | Add -- m*n, m*n -> m*n
  | Scale -- 1*1, m*n -> m*n
  | Dot -- m*n, n*m -> m*m
  | Cross -- m*n, m*n -> m*n
  | Transpose -- m*n -> n*m
  | Inverse -- n*n -> n*n
  | Determinant -- n*n -> 1*1
  | Ones Int Int -- forall m, n. m*n
  | Zeros Int Int -- forall m, n. m*n

derive instance Generic ExprLabel _

instance Show ExprLabel where
  show x = genericShow x

instance Eq ExprLabel where
  eq x = genericEq x

matrix m = Tree (Matrix m) []
scalar s = Tree (Scalar s) []
add m1 m2 = Tree Add [ m1, m2 ]
scale s m = Tree Scale [ s, m ]
dot m1 m2 = Tree Dot [ m1, m2 ]
cross m1 m2 = Tree Cross [ m1, m2 ]
trans m = Tree Transpose [ m ]
inv m = Tree Inverse [ m ]
det m = Tree Determinant [ m ]
ones m n = Tree (Ones m n) []
zeros m n = Tree (Zeros m n) []

-- =============================================================================
-- Rule

data Rule = Rule String (Expr -> Maybe Expr)

