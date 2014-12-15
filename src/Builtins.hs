module Builtins where
import Eval
import EvalAST
import System.IO
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

makeBuiltin :: Int -> (Value -> Value) -> Value
makeBuiltin id func = BlockValue id id [] (Scope Map.empty [id]) (Block Nothing (VarBind "x") (Yield "return" (Builtin (VarAccess "x") (BuiltinFunc func)) noop))

loop :: Value
loop = BlockValue 1 1 [] (Scope Map.empty [1]) (Block Nothing (TuplePattern [VarBind "arg", VarBind "block"]) loopBody) where
  loopBody = Advance (VarAccess "block") (VarAccess "arg") [
      ("restart", Block Nothing (TuplePattern [VarBind "arg", NobodyCares]) loopBody),
      ("end", Block Nothing NobodyCares (Yield "return" (TupleLiteral []) noop))
    ] 

runRecursive :: Value
runRecursive =
  let runRecursiveBody = (Advance (VarAccess "block") (VarAccess "arg") [
          ("recurse", Block Nothing (TuplePattern [VarBind "newArg", VarBind "block"]) (
              Advance (VarAccess "runRecursive") (TupleLiteral [VarAccess "newArg", VarAccess "originalBlock"]) [
                  ("return", Block Nothing (TuplePattern [VarBind "arg", NobodyCares]) runRecursiveBody)
                ]
            )),
          ("return", Block Nothing (TuplePattern [VarBind "result", NobodyCares]) (Yield "return" (VarAccess "result") noop)),
          ("end", Block Nothing NobodyCares (Yield "return" (TupleLiteral []) noop))
        ])
  in BlockValue 2 2 [] (Scope (Map.fromList [("runRecursive", runRecursive)]) [2]) (
      Block Nothing (TuplePattern [VarBind "arg", VarBind "block"]) (Bind (VarAccess "block") (Block Nothing (VarBind "originalBlock") runRecursiveBody))
    )

builtins :: Map.Map String Value
builtins = Map.fromList [
    ("loop", loop),
    ("runRecursive", runRecursive),
    ("toInt", makeBuiltin 3 (\(StringValue s) -> case readMaybe s of
      Just x -> TupleValue [BoolValue True, IntValue x]
      Nothing -> TupleValue [BoolValue False, StringValue ("toInt - Not an integer: " ++ s)]))
  ]

rootBlockId = 1 + length (Map.toList builtins)

printListeners, readListeners, endListeners :: [(String, Int, Int, Scope, Block)]
printListeners = [("print", rootBlockId, -1, Scope Map.empty [], Block Nothing (TuplePattern [VarBind "toPrint", VarBind "cont"]) EmptyBlock)]
readListeners = [("read", rootBlockId, -2, Scope Map.empty [], Block Nothing (TuplePattern [NobodyCares, VarBind "cont"]) EmptyBlock)]
endListeners = [("end", rootBlockId, -3, Scope Map.empty [], Block Nothing NobodyCares EmptyBlock), ("exit", rootBlockId, -3, Scope Map.empty [], Block Nothing NobodyCares EmptyBlock)]

makeBuiltinState :: BlockBody -> CTUState
makeBuiltinState body = CTUState {
    stateBlockId = rootBlockId,
    stateStack = [printListeners, readListeners, endListeners],
    stateScope = Scope builtins [rootBlockId],
    stateBody = body,
    stateIdCounter = 1 + rootBlockId
  }

run :: BlockBody -> IO ()
run body = run' $ makeBuiltinState body

run' state = case stateBlockId state of
  -1 -> do
    let CTUState n stack scope _ idCount = state
        toPrint = vars scope Map.! "toPrint"
    putStr $ repr toPrint
    run' $ nextState $ CTUState (-1) stack scope (Advance (VarAccess "cont") (TupleLiteral []) [("print", Block Nothing (TuplePattern [VarBind "toPrint", VarBind "cont"]) EmptyBlock)]) idCount
  -2 -> do
    let CTUState _ stack scope _ idCount = state
    result <- getLine
    run' $ nextState $ CTUState (-2) stack scope (Advance (VarAccess "cont") (StringLiteral result) [("read", Block Nothing (TuplePattern [NobodyCares, VarBind "cont"]) EmptyBlock)]) idCount
  -3 -> return ()
  _ -> run' $ nextState state