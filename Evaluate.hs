{-# LANGUAGE ExistentialQuantification #-}

module Evaluate where
import           Control.Monad.Error
import           Data.Maybe
import           Types
import GHC.Base
import           Environment
import Parser

data Unpacker = forall a. Eq a => AnyUnpacker (Val -> ThrowsError a)

eval :: Env -> Val -> IOThrowsError Val
eval _ val@(String _)             = return val
eval _ val@(Number _)             = return val
eval _ val@(Bool _)               = return val
eval _ val@(Float _)              = return val
eval env (Atom x) = getVar env x
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
        result <- eval env predicate
        case result of
            Bool False -> eval env alt
            _ -> eval env conseq
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ SpecialForm "no true clauses in cond expression: " form
  else case head clauses of
    List [Atom "else", expr] -> eval env expr
    List [test, expr] -> eval env $ List [Atom "if", test, expr,
                                      List (Atom "cond" : tail clauses)]
    _ -> throwError $ SpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ SpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> liftM last $ mapM (eval env) exprs
    List (List datums : exprs) -> do
      result <- eval env key
      equality <- mapM (\x -> liftT (eqv [result, x])) datums
      if Bool True `elem` equality
        then liftM last $ mapM (eval env) exprs
        else eval env $ List (Atom "case" : key: tail clauses)
    _ -> throwError $ SpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : fparams) : bod)) =
     makeNormalFunc env fparams bod >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : fparams) varArgs : bod)) =
    makeVarArgs varArgs env fparams bod >>= defineVar env var
eval env (List (Atom "lambda" : List fparams : bod)) =
     makeNormalFunc env fparams bod
eval env (List (Atom "lambda" : DottedList fparams varArgs : bod)) =
    makeVarArgs varArgs env fparams bod
eval env (List (Atom "lambda" : varArgs@(Atom _) : bod)) =
     makeVarArgs varArgs env [] bod
eval env (List [Atom "load", String name]) =
     load name >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
     f <- eval env func
     argVals <- mapM (eval env) args
     apply f argVals
eval _ badForm = throwError $ SpecialForm "Unrecognized special form" badForm


makeFunc :: Maybe String -> Env -> [Val] -> [Val] -> IOThrowsError Val
makeFunc varArgs env fparams bod = return $ Func (map showVal fparams) varArgs bod env

makeNormalFunc :: Env -> [Val] -> [Val] -> IOThrowsError Val
makeNormalFunc = makeFunc Nothing

makeVarArgs :: Val -> Env -> [Val] -> [Val] -> IOThrowsError Val
makeVarArgs = makeFunc . Just . showVal

apply :: Val -> [Val] -> IOThrowsError Val
apply (SFunc func) args = liftT $ func args
apply (IOFunc func) args = func args
apply (Func fparams varArgs bod fclosure) args =
  if num fparams /= num args && isNothing varArgs
    then throwError $ ArgNumber (num fparams) args
    else liftIO (bindVars fclosure $ zip fparams args) >>= bindVarArgs varArgs >>= evalBody
  where remainingArgs = drop (length fparams) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) bod
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply _ _ = assert False undefined

eqv :: [Val] -> ThrowsError Val
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [ll@(List _), rl@(List _)]         = eqvList eqv [ll, rl]
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ ArgNumber 2 badArgList

eqvList :: ([Val] -> ThrowsError Val) -> [Val] -> ThrowsError Val
eqvList eqvFunc [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                                      all eqvPair (zip arg1 arg2)
      where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                Left _ -> False
                                Right (Bool val) -> val
                                Right _ -> assert False undefined
eqvList _ _ = assert False undefined

load :: String -> IOThrowsError [Val]
load name = liftIO (readFile name) >>= liftT . lispParseList
