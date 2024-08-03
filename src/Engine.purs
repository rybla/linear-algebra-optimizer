module Engine where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, execStateT, get, modify_)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Record as Record
import Types (Expr, Rule(..), Tree(..))
import Utility (unreachable)

type Weight = Number

type Ctx =
  { rules :: Array Rule
  , weightExpr :: Expr -> Aff Weight
  }

type Env =
  { bestExprWeight :: Weight
  , bestExpr :: Expr
  }

run ctx env expr = do
  bestExprWeight <- ctx.weightExpr expr # liftAff
  step identity expr
    # flip runReaderT
        ctx
    # flip execStateT
        ( Record.merge env
            { bestExpr: expr, bestExprWeight }
        )

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
  { weightExpr } <- ask
  exprs_and_weights <- rules
    # traverse
        ( \rule -> tryRule rule expr >>= case _ of
            Nothing -> pure Nothing
            Just expr' -> do
              let expr'Top = wrap expr'
              expr'Top_weight <- expr'Top # weightExpr # liftAff
              pure (Just (expr' /\ expr'Top /\ expr'Top_weight))
        )
    # map (Array.foldMap Array.fromFoldable)
  case Array.sortBy (\(_ /\ _ /\ w1) (_ /\ _ /\ w2) -> w1 `compare` w2) exprs_and_weights # Array.head of
    Nothing -> pure Nothing
    Just (expr' /\ _ /\ _) -> do
      updateBestExpr expr' # void
      pure (Just expr')

tryRule :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Rule -> Expr -> m (Maybe Expr)
tryRule (Rule f) expr = pure (f expr)

updateBestExpr :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Expr -> m Unit
updateBestExpr expr = do
  { weightExpr } <- ask
  weight <- weightExpr expr # liftAff
  { bestExprWeight } <- get
  when (weight < bestExprWeight) (modify_ _ { bestExprWeight = weight, bestExpr = expr })
  pure unit

