module Main where
import Eval
import Parse (fileParser)
import ASTConvert (convertStmts)
import Builtins (run)
import qualified EvalAST as E
import qualified ParseAST as P
import Text.Parsec (parse)
import System.IO
import System.Environment (getArgs)
import Paths_continue (getDataFileName)

getStdlib :: IO (Maybe [P.Statement])
getStdlib = do
  path <- getDataFileName "stdlib.ctu"
  code <- readFile path
  case parse fileParser "stdlib" code of
    Right ast -> return $ Just ast
    Left err -> do
      putStrLn $ show err
      return Nothing

go :: String -> IO ()
go path = do
  code <- readFile path
  case parse fileParser path code of
    Right ast -> do
      maybeStdlib <- getStdlib
      case maybeStdlib of
        Just stdlib -> do
          let converted = convertStmts (stdlib ++ ast)
          run converted
        Nothing -> return ()
    Left err -> do
      putStrLn $ show err
      return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> go filename
    _ -> putStrLn "USAGE: ctu <script filename>"