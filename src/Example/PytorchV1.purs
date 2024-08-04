module Example.PytorchV1 where

import Prelude hiding (add)

import Control.Monad.State (execStateT)
import Control.Monad.State.Class (modify_)
import Data.Array (zipWith)
import Data.Array as Array
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Semiring as Semiring
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import EngineV2 as Engine
import FileSystem as FS
import Process as Process
import Types (Expr, ExprLabel(..), Rule(..), S, Tree(..), Weight, add, matrix, ones)
import Utility (unreachable)

main :: Effect Unit
main = launchAff_ do
  Console.log "[main]"
  
  -- calcWeight <- make_calcWeight_torchCompileLineCount
  let calcWeight = calcWeight_treeSize
  ctx <- pure
    { rules
    , calcWeight
    , depth: 3
    }
  
  env <- pure {}
  
  expr <- pure
    ( matrix [ [ 1.0, 2.0 ], [ 3.0, 4.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` ones 2 2
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
        `add` matrix [ [ 5.0, 6.0 ], [ 7.0, 8.0 ] ]
    )
  
  _env' <- Engine.run ctx env expr
  
  pure unit

rules :: Array Rule
rules =
  [ Rule "matrix addition of literals"
      case _ of
        Tree Add [ Tree (Matrix m1) [], Tree (Matrix m2) [] ] -> Just (Tree (Matrix (zipWith (zipWith Semiring.add) m1 m2)) [])
        _ -> Nothing
  , Rule "matrix addition associativity"
      case _ of
        Tree Add [ Tree (Matrix _) [], Tree Add [ Tree (Matrix _) [], Tree (Matrix _) [] ] ] -> Nothing
        Tree Add [ Tree Add [ Tree (Matrix _) [], Tree (Matrix _) [] ], Tree (Matrix _) [] ] -> Nothing
        Tree Add [ m1@(Tree (Matrix _) []), Tree Add [ m2@(Tree (Matrix _) []), m3 ] ] -> Just (Tree Add [ Tree Add [ m1, m2 ], m3 ])
        Tree Add [ Tree Add [ m1, m2@(Tree (Matrix _) []) ], m3@(Tree (Matrix _) []) ] -> Just (Tree Add [ m1, Tree Add [ m2, m3 ] ])
        _ -> Nothing
  , Rule "matrix addition commutativity"
      case _ of
        -- commute matrix literals to the right
        Tree Add [ _, Tree (Matrix _) [] ] -> Nothing
        Tree Add [ Tree (Matrix _) [], Tree (Matrix _) [] ] -> Nothing
        Tree Add [ m1@(Tree (Matrix _) []), m2 ] -> Just (Tree Add [ m2, m1 ])
        _ -> Nothing
  ]

calcWeight_treeSize :: Expr -> Aff Weight
-- example: count number of nodes
calcWeight_treeSize expr = expr # traverse (const (modify_ (_ + 1.0))) # flip execStateT 0.0

make_calcWeight_torchCompileLineCount :: Aff (Expr -> Aff Weight)
make_calcWeight_torchCompileLineCount = do
  counter_ref <- 0 # Ref.new # liftEffect
  pure (calcWeight_torchCompileLineCount counter_ref)

calcWeight_torchCompileLineCount :: Ref Int -> Expr -> Aff Weight
-- example: count number of lines in torch-compiled C
calcWeight_torchCompileLineCount counter_ref expr = do
  counter <- counter_ref # Ref.modify (_ + 1) # liftEffect
  compile ("expr" <> show counter) expr
    # map (String.split (String.Pattern "\n") >>> length)

python_dir :: String 
python_dir = "./python/"

make_python_filename :: String -> String
make_python_filename name = python_dir <> name <> ".py"

make_log_filename :: String -> String
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
  , "def expr():"
  , "    return " <> goExpr expr0
  , ""
  , "compiled_expr = torch.compile(expr)"
  , "compiled_expr()"
  ] # Array.intercalate "\n"
  where
  goExpr = case _ of
    Tree (Matrix m) [] -> "np.matrix(" <> show m <> ")"
    Tree (Scalar s) [] -> goS s
    Tree Add [ e1, e2 ] -> "np.add(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Scale [ Tree (Matrix [ [ s ] ]) [], e2 ] -> "(" <> goS s <> " * " <> goExpr e2 <> ")"
    Tree Dot [ e1, e2 ] -> "np.dot(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Cross [ e1, e2 ] -> "linalg.cross(" <> goExpr e1 <> ", " <> goExpr e2 <> ")"
    Tree Transpose [ e ] -> "linalg.matrix_transpose(" <> goExpr e <> ")"
    Tree Inverse [ e ] -> "linalg.inv(" <> goExpr e <> ")"
    Tree Determinant [ e ] -> "linalg.det(" <> goExpr e <> ")"
    Tree (Ones m n) [] -> "torch.ones(" <> show m <> ", " <> show n <> ")"
    Tree (Zeros m n) [] -> "torch.zeros(" <> show m <> ", " <> show n <> ")"
    expr -> unreachable $ "invalid expr: " <> show expr

  goS :: S -> String
  goS = show

