module ParseAST where
import EvalAST (InfixOperator, PrefixOperator)

data Expr = IntLiteral Integer
          | StringLiteral String
          | BoolLiteral Bool
          | BlockLiteral Block
          | TupleLiteral [Expr]
          | VarAccess String
          | IfElse Expr Expr Expr
          | PrefixOperation PrefixOperator Expr
          | InfixOperation InfixOperator Expr Expr
          | Yield String Expr
          | QualifiedYield Expr String Expr
          | FCall Expr Expr
          deriving (Show)

data LValue = TuplePattern [LValue]
            | VarBind String
            | NobodyCares
            deriving (Show)

data Statement = Bind LValue Expr
               | Advance Expr Expr [(String, LValue, [Statement])]
               | ExprStatement Expr
               deriving (Show)

-- Block along with maybe a name to bind that block ID to and
-- an lvalue to bind the block's argument to
data Block = Block (Maybe String) LValue [Statement]
  deriving (Show)
