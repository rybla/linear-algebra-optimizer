module Example.PytorchV1 where

import Prelude hiding (add)

import Data.Array (zipWith)
import Data.Array as Array
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Semiring as Semiring
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Engine (Weight)
import Engine as Engine
import FileSystem as FS
import Process as Process
import Types (Expr, ExprLabel(..), Rule(..), Tree(..), S, add, lit)
import Utility (unreachable)

main :: Effect Unit
main = launchAff_ do
  Console.log "[main]"
  counter_ref <- 0 # Ref.new # liftEffect
  ctx <- pure { rules, calcWeight: calcWeight counter_ref }
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

calcWeight :: Ref Int -> Expr -> Aff Weight
-- example: count number of lines in torch-compiled C
calcWeight counter_ref expr = do
  counter <- counter_ref # Ref.modify (_ + 1) # liftEffect
  compile ("expr" <> show counter) expr
    # map (String.split (String.Pattern "\n") >>> length)

python_dir = "./python/"
make_python_filename name = python_dir <> name <> ".py"
make_log_filename name = python_dir <> name <> ".log.txt"

compile :: String -> Expr -> Aff String
compile name expr = do
  -- translate to Python: evaluate the expression, use Pytorch compile
  let python_code = compileToPython expr
  -- Console.log ("```python\n" <> python_code <> "\n```")
  let python_filename = make_python_filename name
  let log_filename = make_log_filename name
  FS.write python_filename python_code
  -- translate to C (printed to log_filename)
  Process.spawn [ "sh", "./run_python.sh", python_filename, log_filename ] # void # liftAff
  let log_file = FS.read log_filename
  log_text <- FS.text log_file # liftAff
  pure log_text

compileToPython :: Expr -> String
compileToPython expr0 =
  [ "import numpy as np"
  , "import numpy.linalg as linalg"
  , "import torch"
  , ""
  , "def expr():\n    return " <> goExpr expr0
  , ""
  , "compiled_expr = torch.compile(expr)"
  , "compiled_expr()"
  ] # Array.intercalate "\n"
  where
  goExpr = case _ of
    Tree (Lit m) [] -> "np.matrix(" <> show m <> ")"
    Tree Add [ e1, e2 ] -> "np.add(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Scale [ Tree (Lit [ [ s ] ]) [], e2 ] -> "(" <> goS s <> " * " <> goExpr e2 <> ")"
    Tree Dot [ e1, e2 ] -> "np.dot(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Cross [ e1, e2 ] -> "linalg.cross(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Transpose [ e ] -> "linalg.matrix_transpose(" <> goExpr e <> ")"
    Tree Inverse [ e ] -> "linalg.inv(" <> goExpr e <> ")"
    Tree Determinant [ e ] -> "linalg.det(" <> goExpr e <> ")"
    expr -> unreachable $ "invalid expr: " <> show expr

  goS :: S -> String
  goS = show

