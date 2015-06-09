module LispPrimitives where
import Types
import LispIO
import Environment
import Evaluate
import Control.Monad.Error

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunction IOFunc) ioPrimitives
                                              ++ map (makeFunction SFunc) primitives)
      where makeFunction constructor (var, func) = (var, constructor func)

primitives :: [(String, [Val] -> ThrowsError Val)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [Val] -> ThrowsError Val
numericBinop _ []            = throwError $ ArgNumber 2 []
numericBinop _ singleVal@[_] = throwError $ ArgNumber 2 singleVal
numericBinop op fparams       = liftM (Number . foldl1 op) (mapM unpackNum fparams)

unaryOp :: (Val -> Val) -> [Val] -> ThrowsError Val
unaryOp f [val]              = return $ f val
unaryOp _ []               = throwError $ ArgNumber 1 []
unaryOp _ dL@(_ : (_ : _)) = throwError $ Type "List" (head dL)

boolBinop :: (Val -> ThrowsError a) -> (a -> a -> Bool) -> [Val] -> ThrowsError Val
boolBinop unpacker op args = if length args /= 2
                             then throwError $ ArgNumber 2 args
                             else do
                               left <- unpacker $ head args
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [Val] -> ThrowsError Val
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Val] -> ThrowsError Val
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Val] -> ThrowsError Val
boolBoolBinop = boolBinop unpackBool

unpackNum :: Val -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                             then throwError $ Type "Number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ Type "Number" notNum

unpackStr :: Val -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ Type "String" notString

unpackBool :: Val -> ThrowsError Bool
unpackBool (Bool val) = return val
unpackBool notBool  = throwError $ Type "Bool" notBool

symbolp, numberp, stringp, boolp, listp, symbol2string, string2symbol :: Val -> Val
symbolp (Atom _)         = Bool True
symbolp _                = Bool False
numberp (Number _)       = Bool True
numberp _                = Bool False
stringp (String _)       = Bool True
stringp _                = Bool False
boolp (Bool _)           = Bool True
boolp _                  = Bool False
listp (List _)           = Bool True
listp _                  = Bool False
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

car :: [Val] -> ThrowsError Val
car [List (x       : _)]   = return x
car [badArg]               = throwError $ Type "Pair" badArg
car badArgList             = throwError $ ArgNumber 1 badArgList

cdr :: [Val] -> ThrowsError Val
cdr [List (_       : xs)]   = return $ List xs
cdr [badArg]                = throwError $ Type "Pair" badArg
cdr badArgList              = throwError $ ArgNumber 1 badArgList

cons :: [Val] -> ThrowsError Val
cons [xL, List []]            = return $ List [xL]
cons [x, List xs]             = return $ List $ x : xs
cons badArgList               = throwError $ ArgNumber 2 badArgList

unpackEquals :: Val -> Val -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
          do unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2
      `catchError` const (return False)

equal :: [Val] -> ThrowsError Val
equal [ll@(List _), rl@(List _)]         = eqvList equal [ll, rl]
equal [arg1, arg2]                       = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum,AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList                         = throwError $ ArgNumber 2 badArgList
