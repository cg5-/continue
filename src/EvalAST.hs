module EvalAST where
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

data PrefixOperator = Not | Negate deriving (Eq, Ord)

prefixOperatorTable :: Map.Map PrefixOperator (Int, String)
prefixOperatorTable = Map.fromList [
    (Not,    (1, "!")),
    (Negate, (1, "-"))
  ]

instance Show PrefixOperator where
  show op = snd $ prefixOperatorTable Map.! op

data InfixOperator = Times | Divide | Modulo | Concat | Plus | Minus | Eq | NotEq | Less | LessEq | Greater | GreaterEq | And | Or
                   deriving (Eq, Ord)

infixOperatorTable :: Map.Map InfixOperator (Int, String)
infixOperatorTable = Map.fromList [
    (Times,     (4, "*")),
    (Divide,    (4, "/")),
    (Modulo,    (4, "%")),
    (Concat,    (3, "++")),
    (Plus,      (3, "+")),
    (Minus,     (3, "-")),
    (Eq,        (2, "==")),
    (NotEq,     (2, "/=")),
    (Less,      (2, "<")),
    (LessEq,    (2, "<=")),
    (Greater,   (2, ">")),
    (GreaterEq, (2, ">=")),
    (And,       (1, "&&")),
    (Or,        (1, "||"))
  ]

instance Show InfixOperator where
  show op = snd $ infixOperatorTable Map.! op

----------

newtype BuiltinFunc = BuiltinFunc (Value -> Value)

data Expr = IntLiteral Integer
          | StringLiteral String
          | BoolLiteral Bool
          | BlockLiteral Block
          | TupleLiteral [Expr]
          | VarAccess String
          | IfElse Expr Expr Expr
          | PrefixOperation PrefixOperator Expr
          | InfixOperation InfixOperator Expr Expr
          | Builtin Expr BuiltinFunc
          deriving (Show)

instance Show BuiltinFunc where
  show f = "<builtin>"

data LValue = TuplePattern [LValue]
            | VarBind String
            | NobodyCares

instance Show LValue where
  show NobodyCares = "_"
  show (VarBind x) = x
  show (TuplePattern lvals) = "(" ++ intercalate ", " (map show lvals) ++ ")"

data BlockBody = EmptyBlock
               | Bind Expr Block
               | Yield String Expr Block
               | QualifiedYield Expr String Expr Block
               | Advance Expr Expr [(String, Block)]
               deriving (Show)

-- Block along with maybe a name to bind that block ID to and
-- an lvalue to bind the block's argument to
data Block = Block (Maybe String) LValue BlockBody
  deriving (Show)

--instance Show Block where
--  show block = "<block>"

-- Runtime values

data Value = IntValue Integer
           | StringValue String
           | BoolValue Bool
           | TupleValue [Value]
           | BlockValue Int Int Stack Scope Block -- Int Int -- upper ID, lower ID
           | BlockIdValue Int

instance Eq Value where
  (IntValue a) == (IntValue b) = a == b
  (StringValue a) == (StringValue b) = a == b
  (BoolValue a) == (BoolValue b) = a == b
  (TupleValue a) == (TupleValue b) = a == b
  (BlockValue _ _ _ _ _) == (BlockValue _ _ _ _ _) = error "Can't compare two blocks"
  (BlockIdValue a) == (BlockIdValue b) = a == b
  _ == _ = False

instance Ord Value where
  (IntValue a) <= (IntValue b) = a <= b
  (StringValue a) <= (StringValue b) = a <= b
  (BoolValue a) <= (BoolValue b) = a <= b
  (TupleValue a) <= (TupleValue b) = a <= b
  a <= b = error $ "Can't compare " ++ show a ++ " and " ++ show b

instance Show Value where
  show (IntValue n) = show n
  show (StringValue s) = "\"" ++ s ++ "\""
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"
  show (TupleValue values) = "(" ++ intercalate ", " (map show values) ++ ")"
  show (BlockValue upperId lowerId _ _ _) = "<block " ++ show upperId ++ "/" ++ show lowerId ++ ">"
  show (BlockIdValue id) = "<id " ++ show id ++ ">"

repr :: Value -> String
repr (StringValue s) = s
repr x = show x

-- Each stack frame is a list of "listeners" of the form
-- (listenerName, inBlockId, outBlockId, outScope, outBlock)
-- e.g. ("return", 4, 2, scope, block)
--      if a block with ID 4 "return"s, then we pick up at that block with ID 2 and that scope
type Stack = [[(String, Int, Int, Scope, Block)]]

-- Scope. Contains a map of variable names to their values, and a list of block ID's
-- for the blocks it is lexically nested inside (including its own)
data Scope = Scope {
  vars :: Map.Map String Value,
  nestedBlockIds :: [Int]
} deriving (Eq)