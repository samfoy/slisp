module Types where
import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec(ParseError)
import GHC.Base
import System.IO

-- Lisp Types

data Val = Atom String
         | String String
         | Number Integer
         | Float Float
         | Bool Bool
         | Char Char
         | List [Val]
         | DottedList [Val] Val
         | SFunc ([Val] -> ThrowsError Val)
         | IOFunc ([Val] -> IOThrowsError Val)
         | Port Handle
         | Func { p :: [String], v :: Maybe String,
                  b :: [Val], c :: Env}

instance Show Val where show = showVal

instance Eq Val where
  (==) (Atom s1) (Atom s2) = s1 == s2
  (==) (String s1) (String s2) = s1 == s2
  (==) (Number n1) (Number n2) = n1 == n2
  (==) (Float f1) (Float f2) = f1 == f2
  (==) (Bool b1) (Bool b2) = b1 == b2
  (==) (List l1) (List l2) = all eqvPair (zip l1 l2)
                where eqvPair (x1, x2) = (==) x1 x2
  (==) (DottedList xs x) (DottedList ys y) = (==) (List $ xs ++ [x]) (List $ ys ++ [y])
  (==) _ _ = assert False undefined

-- Error Types

data LispError = ArgNumber Integer [Val]
               | Type String Val
               | Parse ParseError
               | NotFunc String String
               | VarNotBound String String
               | SpecialForm String Val
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

-- Helper Error Functions

trapError :: (MonadError e m, Show e) => m String -> m String
trapError a = catchError a (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = assert False undefined

liftT :: ThrowsError a -> IOThrowsError a
liftT (Right val) = return val
liftT (Left err) = throwError err

runIOThrows :: IOThrowsError String -> IO String
runIOThrows s = liftM extractValue (runExceptT (trapError s))

-- Closure Types

type Env = IORef [(String, IORef Val)]

-- Instance Show Functions

showVal :: Val -> String
showVal (Atom s) = s
showVal (String s) = s
showVal (Number n) = show n
showVal (Float n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char n) = show n
showVal (List l) = "(" ++ unpackList l ++ ")"
showVal (DottedList lHead lTail) = "(" ++ unpackList lHead ++ " . " ++ showVal lTail ++ ")"
showVal (SFunc _) = "<primitive>"
showVal (Port _)                 = "<IO port>"
showVal (IOFunc _)               = "<IO primitives>"
showVal (Func {p = args, v = varArgs, b = _, c = _}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varArgs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"

showError :: LispError -> String
showError (ArgNumber ex f) = "Expected " ++ show ex ++ " arguments; but found "
                           ++ unpackList f
showError (Type ex f) = "Improper Type: expected " ++ ex ++ ", found " ++ show f
showError (Parse x) = "Parser error at " ++ show x
showError (NotFunc m f) = m ++ ": " ++ show f
showError (VarNotBound m x) = m ++ ": " ++ x
showError (SpecialForm m f) = m ++ ": " ++ show f
showError (Default s) = s

-- Helper Functions for Show Functions

unpackList :: [Val] -> String
unpackList = unwords . map showVal
