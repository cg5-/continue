module ASTConvert where
import qualified EvalAST as E
import qualified ParseAST as P
import Control.Monad.State

-- function taking blockbody and prepending a statement or multiple statements
type StatementWrapper = E.BlockBody -> E.BlockBody

type ConvState = (StatementWrapper, Int)

gensym :: State ConvState String
gensym = do
  (stmts, n) <- get
  put (stmts, n + 1)
  return $ "_" ++ show n

pushStatement :: StatementWrapper -> State ConvState ()
pushStatement stmt = do
  (stmts, n) <- get
  put (stmts . stmt, n)
  return ()

----------

convertExpr :: P.Expr -> State ConvState E.Expr
convertExpr (P.IntLiteral n) = return $ E.IntLiteral n
convertExpr (P.StringLiteral n) = return $ E.StringLiteral n
convertExpr (P.BoolLiteral n) = return $ E.BoolLiteral n
convertExpr (P.VarAccess n) = return $ E.VarAccess n
convertExpr (P.BlockLiteral b) = return $ E.BlockLiteral $ convertBlock b
convertExpr (P.TupleLiteral exprs) = do
  converted <- mapM convertExpr exprs
  return $ E.TupleLiteral converted

convertExpr (P.IfElse cond ifTrue ifFalse) = do
  convertedCond <- convertExpr cond
  convertedTrue <- convertExpr ifTrue
  convertedFalse <- convertExpr ifFalse
  return $ E.IfElse convertedCond convertedTrue convertedFalse

convertExpr (P.PrefixOperation operator operand) = do
  convertedOperand <- convertExpr operand
  return $ E.PrefixOperation operator convertedOperand

convertExpr (P.InfixOperation op l r) = do
  convertedL <- convertExpr l
  convertedR <- convertExpr r
  return $ E.InfixOperation op convertedL convertedR

convertExpr (P.Yield listenerName arg) = do
  convertedArg <- convertExpr arg
  temporaryName <- gensym
  pushStatement $ \body -> E.Yield listenerName convertedArg (E.Block Nothing (E.VarBind temporaryName) body)
  return $ E.VarAccess temporaryName

convertExpr (P.QualifiedYield id listenerName arg) = do
  convertedId <- convertExpr id
  convertedArg <- convertExpr arg
  temporaryName <- gensym
  pushStatement $ \body -> E.QualifiedYield convertedId listenerName convertedArg (E.Block Nothing (E.VarBind temporaryName) body)
  return $ E.VarAccess temporaryName

convertExpr (P.FCall func arg) = do
  convertedFunc <- convertExpr func
  convertedArg <- convertExpr arg
  temporaryName <- gensym
  pushStatement $ \body -> E.Advance convertedFunc convertedArg [("return", E.Block Nothing (E.TuplePattern [(E.VarBind temporaryName), E.NobodyCares]) body)]
  return $ E.VarAccess temporaryName

convertLValue :: P.LValue -> E.LValue
convertLValue P.NobodyCares = E.NobodyCares
convertLValue (P.VarBind varName) = E.VarBind varName
convertLValue (P.TuplePattern patterns) = E.TuplePattern $ map convertLValue patterns

convertStmt :: P.Statement -> StatementWrapper
convertStmt (P.Bind lval (P.Yield listenerName arg)) =
  let (convertedArg, (stmts, _)) = runState (convertExpr arg) (id, 1)
      convertedLVal = convertLValue lval
  in stmts . \body -> E.Yield listenerName convertedArg (E.Block Nothing convertedLVal body)

convertStmt (P.Bind lval (P.QualifiedYield idExpr listenerName arg)) =
  let (convertedId, (stmts, n)) = runState (convertExpr idExpr) (id, 1)
      (convertedArg, (stmts2, _)) = runState (convertExpr arg) (id, n)
      convertedLVal = convertLValue lval
  in stmts . stmts2 . \body -> E.QualifiedYield convertedId listenerName convertedArg (E.Block Nothing convertedLVal body)

convertStmt (P.Bind lval (P.FCall func arg)) =
  let (convertedFunc, (stmts, n)) = runState (convertExpr func) (id, 1)
      (convertedArg, (stmts2, _)) = runState (convertExpr arg) (id, n)
      convertedLVal = convertLValue lval
  in stmts . stmts2 . \body -> E.Advance convertedFunc convertedArg [("return", E.Block Nothing (E.TuplePattern [convertedLVal, E.NobodyCares]) body)]

convertStmt (P.Bind lval expr) =
  let (convertedExpr, (stmts, _)) = runState (convertExpr expr) (id, 1)
      convertedLVal = convertLValue lval
  in stmts . \body -> E.Bind convertedExpr (E.Block Nothing convertedLVal body)

convertStmt (P.Advance func arg listeners) =
  let (convertedFunc, (stmts, n)) = runState (convertExpr func) (id, 1)
      (convertedArg, (stmts2, _)) = runState (convertExpr arg) (id, n)
  in stmts . stmts2 . \body -> E.Advance convertedFunc convertedArg (map (convertListener body) listeners)
  where
    convertListener :: E.BlockBody -> (String, P.LValue, [P.Statement]) -> (String, E.Block)
    convertListener body (listenerName, lval, statements) =
      let convertedLVal = convertLValue lval
      in (listenerName, E.Block Nothing convertedLVal (foldr ($) body (map convertStmt statements)))
      
convertStmt (P.ExprStatement expr) = convertStmt (P.Bind P.NobodyCares expr)

convertStmts :: [P.Statement] -> E.BlockBody
convertStmts statements = foldr ($) E.EmptyBlock (map convertStmt statements)

convertBlock :: P.Block -> E.Block
convertBlock (P.Block maybeIdName arg statements) = E.Block maybeIdName (convertLValue arg) (convertStmts statements)
