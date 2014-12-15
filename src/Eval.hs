module Eval where
import EvalAST
import qualified Data.Map.Strict as Map
import Data.List (find, intercalate)
import Control.Monad.State

noop :: Block
noop = Block Nothing NobodyCares EmptyBlock

----------

applyPrefixOperation :: PrefixOperator -> Value -> Value
applyPrefixOperation Not (BoolValue b) = BoolValue $ not b
applyPrefixOperation Negate (IntValue i) = IntValue $ -i
applyPrefixOperation op arg = error $ "Invalid operand for " ++ show op ++ ": " ++ show arg 

applyInfixOperation :: InfixOperator -> Value -> Value -> Value
applyInfixOperation Times (IntValue a) (IntValue b) = IntValue $ a * b
applyInfixOperation Divide (IntValue a) (IntValue b) = IntValue $ a `div` b
applyInfixOperation Modulo (IntValue a) (IntValue b) = IntValue $ a `mod` b
applyInfixOperation Concat a b = StringValue $ repr a ++ repr b
applyInfixOperation Plus (IntValue a) (IntValue b) = IntValue $ a + b
applyInfixOperation Minus (IntValue a) (IntValue b) = IntValue $ a - b
applyInfixOperation Eq a b = BoolValue $ a == b
applyInfixOperation NotEq a b = BoolValue $ a /= b
applyInfixOperation Less a b = BoolValue $ a < b
applyInfixOperation LessEq a b = BoolValue $ a <= b
applyInfixOperation Greater a b = BoolValue $ a > b
applyInfixOperation GreaterEq a b = BoolValue $ a >= b
applyInfixOperation And (BoolValue a) (BoolValue b) = BoolValue $ a && b
applyInfixOperation Or (BoolValue a) (BoolValue b) = BoolValue $ a || b
applyInfixOperation op arg1 arg2 = error $ "Invalid operands for " ++ show op ++ ": " ++ show arg1 ++ ", " ++ show arg2

----------

instance Show Scope where
  show scope = "{" ++ (intercalate ", " [k ++ " = " ++ show v | (k,v) <- Map.toList $ vars scope]) ++ "; " ++ show (nestedBlockIds scope) ++ "}"

-- Given a scope, an lvalue and a value, return that same scope, but with the the lvalue bound to the value
bind :: Scope -> LValue -> Value -> Scope
bind old pattern val = Scope (bind' (vars old) pattern val) (nestedBlockIds old) where
  bind' :: Map.Map String Value -> LValue -> Value -> Map.Map String Value
  bind' old NobodyCares _ = old
  bind' old (VarBind varName) val = Map.insert varName val old
  bind' old (TuplePattern patterns) (TupleValue values)
    | length patterns == length values
        = foldl (\vars (pattern, val) -> bind' vars pattern val) old (zip patterns values)
    | otherwise = error $ "Pattern match failed, matching " ++ show patterns ++ " to " ++ show values
  bind' _ p v = error $ "Pattern match failed, matching " ++ show p ++ " to " ++ show v

getAndIncrement :: State Int Int
getAndIncrement = do
  id <- get
  put $ id + 1
  return id

-- Evaluate an expression in some scope. The state monad is used to assign
-- unique IDs to blocks as they're encountered
evaluate :: Scope -> Expr -> State Int Value
evaluate _ (IntLiteral n) = return $ IntValue n
evaluate _ (StringLiteral s) = return $ StringValue s
evaluate _ (BoolLiteral b) = return $ BoolValue b

evaluate scope (IfElse cond ifTrue ifFalse) = do
  evaluatedCond <- evaluate scope cond
  evaluatedTrue <- evaluate scope ifTrue
  evaluatedFalse <- evaluate scope ifFalse
  return $ case evaluatedCond of
    BoolValue True -> evaluatedTrue
    BoolValue False -> evaluatedFalse

evaluate scope (PrefixOperation op arg) = do
  evaluatedArg <- evaluate scope arg
  return $ applyPrefixOperation op evaluatedArg

evaluate scope (InfixOperation op arg1 arg2) = do
  evaluatedArg1 <- evaluate scope arg1
  evaluatedArg2 <- evaluate scope arg2
  return $ applyInfixOperation op evaluatedArg1 evaluatedArg2

evaluate scope (TupleLiteral exprs) = do
  values <- mapM (evaluate scope) exprs
  return $ TupleValue values

evaluate scope (VarAccess varName) = return $ case Map.lookup varName (vars scope) of
  Just value -> value
  Nothing -> error $ "Variable not in scope: " ++ varName

evaluate scope (Builtin expr (BuiltinFunc func)) = do
  evaluated <- evaluate scope expr
  return $ func evaluated

evaluate scope (BlockLiteral block) = do
  -- get a unique ID
  blockId <- getAndIncrement
  -- if the block wanted its ID bound to a variable, add it to its scope now
  let newScope = case block of
        Block (Just name) _ _ -> bind scope (VarBind name) (BlockIdValue blockId)
        Block Nothing _ _ -> scope
  -- also add the new block ID to the new block's list of nested block IDs
  return $ BlockValue blockId blockId [] (Scope (vars newScope) (blockId : nestedBlockIds newScope)) block

-- Current state of an executing CTU prog
data CTUState = CTUState {
  stateBlockId :: Int,
  stateStack :: Stack,
  stateScope :: Scope,
  stateBody :: BlockBody,
  stateIdCounter :: Int
}

instance Show CTUState where
  show st = "blockId: " ++ show (stateBlockId st) ++ "\n\n"
            ++ "stack: " ++ simplifiedStack ++ "\n\n"
            ++ "scope: " ++ show (stateScope st) ++ "\n\n"
            ++ "body: " ++ take 100 (show $ stateBody st) ++ "\n\n"
            ++ "idCounter: " ++ show (stateIdCounter st) ++ "\n\n"
    where simplifiedStack = show $ map (map (\(name, inId, outId, _, _) -> (name, inId, outId))) $ stateStack st

makeBlankState :: BlockBody -> CTUState
makeBlankState body = CTUState 0 [] (Scope Map.empty [0]) body 1

-- Advance to the next state
nextState :: CTUState -> CTUState
nextState (CTUState blId stack scope (Bind expr (Block _ pattern body)) idCount) =
  -- Evaluate the expression, bind it into the scope and switch to the next block
  let (value, idCount2) = runState (evaluate scope expr) idCount
  in CTUState blId stack (bind scope pattern value) body idCount2

nextState (CTUState blId stack scope (Advance blockExpr arg listeners) idCount) =
  -- Evaluate the expression for the block being advanced, and make sure it really is a block
  let (blockValue, idCount2) = runState (evaluate scope blockExpr) idCount in case blockValue of
    BlockValue newUpperId newLowerId newStack newScope (Block _ argPattern blockBody) ->
      -- Evaluate the expression for the argument
      let (argValue, idCount3) = runState (evaluate scope arg) idCount2
          -- bind the argument into the destination scope
          finalScope = bind newScope argPattern argValue
          -- construct a list of listeners and add it to the stack, then add all
          -- of the saved stack frames from the block being advanced
          newListeners = [(listenerName, newUpperId, blId, scope, listenerBlock) | (listenerName, listenerBlock) <- listeners]
          joinedStack = newStack ++ (newListeners : stack)
      -- switch to the new block
      in CTUState newLowerId joinedStack finalScope blockBody idCount3

    _ -> error "Tried to advance something that isn't a block"

nextState (CTUState blId stack scope (Yield listenerName yieldExpr contBlock) idCount) =
  -- for a normal yield action, yield only from blocks in the current lexical scope
  doYield (CTUState blId stack scope (Yield listenerName yieldExpr contBlock) idCount) (`elem` nestedBlockIds scope)

nextState (CTUState blId stack scope EmptyBlock idCount) =
  -- at the end of a block, do an "end" yield action, but only from the current block
  doYield (CTUState blId stack scope (Yield "end" (TupleLiteral []) noop) idCount) (== blId)

nextState (CTUState blId stack scope (QualifiedYield blockIdExpr listenerName yieldExpr contBlock) idCount) =
  -- evaluate the block ID expression and make sure it really is a block ID
  let (blockIdValue, idCount2) = runState (evaluate scope blockIdExpr) idCount in case blockIdValue of
    -- do the yield, but only from that particular block
    BlockIdValue qualifiedId -> doYield (CTUState blId stack scope (Yield listenerName yieldExpr contBlock) idCount2) (== qualifiedId)
    _ -> error $ "Qualified yield from something other than block id: " ++ show blockIdValue

-- Do the "yield" action, taking a predicate which says which block IDs are OK
doYield :: CTUState -> (Int -> Bool) -> CTUState
doYield (CTUState blId stack contScope (Yield listenerName yieldExpr contBlock) idCount) idIsOkFunc =
  -- Evaluate the yield argument
  let (yieldVal, idCount2) = runState (evaluate contScope yieldExpr) idCount
      -- Search for a valid listener and the appropriate block to switch to; construct a continuation block
      (newBlockId, newStack, newScope, (Block _ newPattern newBody), contVal) = searchForListener
      -- Bind the yield argument and the continuation block to the destination scope
      finalScope = bind newScope newPattern (TupleValue [yieldVal, contVal])
  -- Do the switch
  in CTUState newBlockId newStack finalScope newBody idCount2

  where
    searchForListener :: (Int, Stack, Scope, Block, Value)
    searchForListener = searchForListener' stack []

    searchForListener' :: Stack -> Stack -> (Int, Stack, Scope, Block, Value)
    searchForListener' [] contStack = error $ "No listener for " ++ listenerName
    searchForListener' (listeners : restOfStack) contStack =
      -- Look through the current stack frame for a valid listener
      case find (\(name, inId, _, _, _) -> listenerName == name && idIsOkFunc inId) listeners of
        -- We found one, return the appropriate destination block and construct a continuation block
        Just (_, inId, outId, scope, block) -> (outId, restOfStack, scope, block, (BlockValue inId blId (reverse contStack) contScope contBlock))
        -- We didn't find one, move up the stack and try again; save the failed stack frame
        -- in the continuation block's saved stack
        Nothing -> searchForListener' restOfStack (listeners : contStack)
