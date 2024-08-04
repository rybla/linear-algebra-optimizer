module Engine where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, execStateT, get, modify_)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe')
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Record as Record
import Types (Expr, Rule(..), Tree(..))
import Utility (unreachable)

type Weight = Number

type Ctx =
  { rules :: Array Rule
  , calcWeight :: Expr -> Aff Weight
  }

type Env =
  { bestExprWeight :: Weight
  , bestExpr :: Expr
  }

run ctx env expr = do
  bestExprWeight <- ctx.calcWeight expr # liftAff
  Console.log "[start engine]"
  Console.log ("initial expr:\n    " <> show expr)
  Console.log ("initial bestExprWeight: " <> show bestExprWeight)
  Console.log ""
  res <- step identity expr
    # flip runReaderT
        ctx
    # flip execStateT
        ( Record.merge env
            { bestExpr: expr, bestExprWeight }
        )
  Console.log ""
  Console.log ("final expr:\n    " <> show res.bestExpr)
  Console.log ("final bestExprWeight: " <> show res.bestExprWeight)
  Console.log "[end engine]"
  pure res

-- recursively step through entire expression
step :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => (Expr -> Expr) -> Expr -> m Expr
step wrap expr = do
  Tree l es <- stepHere wrap expr
  es' <- es # traverseWithIndex \i ->
    step
      ( wrap <<< \exprKid' ->
          Tree l (es # Array.updateAt i exprKid' # fromMaybe' (\_ -> unreachable "kid index out of bounds"))
      )
  let expr' = Tree l es'
  stepHere wrap expr'

-- step at just the top level of expression
stepHere :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => (Expr -> Expr) -> Expr -> m Expr
stepHere wrap expr = do
  { rules } <- ask
  tryRules rules wrap expr >>= case _ of
    Just expr' -> stepHere wrap expr'
    Nothing -> pure expr

tryRules :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Array Rule -> (Expr -> Expr) -> Expr -> m (Maybe Expr)
tryRules rules wrap expr = do
  { calcWeight } <- ask
  exprs_and_weights <-
    rules
      # parTraverse
          ( \rule@(Rule rulename _) -> case tryRule rule expr of
              Nothing -> pure Nothing
              Just expr' -> do
                Console.log ("used rule " <> show rulename <> ":\n    " <> show expr <> " =>\n    " <> show expr')
                let expr'Top = wrap expr'
                expr'Top_weight <- expr'Top # calcWeight # liftAff
                pure (Just (expr' /\ expr'Top /\ expr'Top_weight))
          )
      # map (Array.foldMap Array.fromFoldable)
      # liftAff
  case Array.sortBy (\(_ /\ _ /\ w1) (_ /\ _ /\ w2) -> w1 `compare` w2) exprs_and_weights # Array.head of
    Nothing -> pure Nothing
    Just (expr' /\ _ /\ weight') -> do
      updateBestExpr expr' weight' # void
      pure (Just expr')

tryRule :: Rule -> Expr -> Maybe Expr
tryRule (Rule _ f) expr = f expr

updateBestExpr :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Expr -> Weight -> m Unit
updateBestExpr expr weight = do
  { bestExprWeight } <- get
  when (weight < bestExprWeight) (modify_ _ { bestExprWeight = weight, bestExpr = expr })
  pure unit

