module EngineV2 where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, execStateT, get, modify_)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Record as Record
import Types (Expr, Rule(..), Weight, showExpr, unfoldTreeWithWrap)

type Ctx =
  { rules :: Array Rule
  , calcWeight :: Expr -> Aff Weight
  , depth :: Int
  }

type Env =
  { bestExprWeight :: Weight
  , bestExpr :: Expr
  }

run ctx env initialExpr = do
  initialExprWeight <- ctx.calcWeight initialExpr # liftAff
  Console.log "[start engine]"
  Console.log ("initial expr:\n    " <> showExpr initialExpr)
  Console.log ("initial bestExprWeight: " <> show initialExprWeight)
  Console.log ""
  res <- loop initialExpr
    # flip runReaderT
        ctx
    # flip execStateT
        ( Record.merge env
            { bestExpr: initialExpr
            , bestExprWeight: initialExprWeight
            }
        )
  Console.log ""
  Console.log ("initial expr:\n    " <> showExpr initialExpr)
  Console.log ("final expr:\n    " <> showExpr res.bestExpr)
  Console.log ("initial initialExprWeight: " <> show initialExprWeight)
  Console.log ("final bestExprWeight: " <> show res.bestExprWeight)
  Console.log "[end engine]"
  pure res

loop :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Expr -> m Unit
loop expr = do
  step expr >>= case _ of
    Nothing -> pure unit
    Just (expr' /\ weight') ->
      updateBestExpr expr' weight' >>=
        if _ then loop expr' else pure unit

step :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Expr -> m (Maybe (Expr /\ Weight))
step expr = do
  { depth, calcWeight } <- ask
  candidateExprs <- nextSteps depth expr
  -- calculate all weights in parallel
  weightedCandidateExprs <-
    candidateExprs
      # parTraverse (\t -> (t /\ _) <$> calcWeight t)
      # liftAff
      # map (Array.sortBy (\(_ /\ w1) (_ /\ w2) -> w1 `compare` w2))
  -- only take lowest-weighted resulting expr from all possible rewrites
  Console.log ""
  Console.log "[weightedCandidateExprs]"
  weightedCandidateExprs # traverse_ \(e /\ w) -> Console.log ("    " <> show w <> " : " <> showExpr e)
  pure (Array.uncons weightedCandidateExprs <#> _.head)

nextSteps :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Int -> Expr -> m (Array Expr)
nextSteps 0 _ = pure []
nextSteps depth expr0 = do
  candidateExprs :: Array Expr <-
    unfoldTreeWithWrap expr0
      # traverse (uncurry tryRules)
      # map Array.fold
  candidateExprs' <-
    candidateExprs
      # traverse (nextSteps (depth - 1))
      # map (Array.foldMap Array.fromFoldable)
  pure (candidateExprs <> candidateExprs')

tryRules :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => (Expr -> Expr) -> Expr -> m (Array Expr)
tryRules wrap expr = do
  { rules } <- ask
  rules
    # traverse
        ( \(Rule rulename tryRule) -> case tryRule expr of
            Nothing -> pure Nothing
            Just expr' -> do
              Console.log ""
              Console.log ("[rule " <> show rulename <> "]\n    " <> showExpr expr <> " =>\n    " <> showExpr expr')
              pure (Just (wrap expr'))
        )
    # map (Array.foldMap Array.fromFoldable)

-- | Returns true if the best expr was updated
updateBestExpr :: forall m. MonadReader Ctx m => MonadState Env m => MonadAff m => Expr -> Weight -> m Boolean
updateBestExpr expr weight = do
  { bestExprWeight } <- get
  if (weight < bestExprWeight) then do
    Console.log ""
    Console.log "[progress]"
    Console.log ("    bestExpr: " <> showExpr expr)
    Console.log ("    bestExprWeight: " <> show weight)
    modify_ _ { bestExprWeight = weight, bestExpr = expr }
    pure true
  else
    pure false

