module Example.SimpleV1 where

import Prelude hiding (add)

import Control.Monad.State (execStateT, modify_)
import Data.Array (zipWith)
import Data.Maybe (Maybe(..))
import Data.Semiring as Semiring
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Engine (Weight)
import Engine as Engine
import Types (ExprLabel(..), Rule(..), Tree(..), Expr, add, lit)

main :: Effect Unit
main = launchAff_ do
  Console.log "[main]"
  ctx <- pure { rules, weightExpr }
  env <- pure {}
  expr <- pure (lit [ [ 1.0 ] ] `add` lit [ [ 2.0 ] ])
  env' <- Engine.run ctx env expr
  Console.log ("env:\n" <> show env')
  pure unit

rules :: Array Rule
rules =
  [ Rule case _ of
      -- Lit m1 + Lit m2 => Lit (m1 + m2)
      Tree Add [ Tree (Lit m1) [], Tree (Lit m2) [] ] -> Just (Tree (Lit (zipWith (zipWith Semiring.add) m1 m2)) [])
      _ -> Nothing
  ]

weightExpr :: Expr -> Aff Weight
-- example: count number of nodes
weightExpr expr = expr # traverse (const (modify_ (_ + 1.0))) # flip execStateT 0.0

