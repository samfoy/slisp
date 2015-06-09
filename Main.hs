module Main(main) where
import Control.Monad.Error
import Types
import Evaluate
import Parser
import Environment
import System.Environment
import System.IO
import LispPrimitives

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftT (lispParse expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) $ action result >> until_ predicate prompt action

runOne :: [String] -> IO ()
runOne exprs = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 exprs)]
  runIOThrows (liftM show $ eval env (List [Atom "load", String (head exprs)]))
      >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
