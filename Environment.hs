module Environment where
import Types
import Data.IORef
import Control.Monad
import Data.Maybe
import Control.Monad.Error

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound ref var = liftM (isJust . lookup var) (readIORef ref)

getVar :: Env -> String -> IOThrowsError Val
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ VarNotBound "Getting an unbound variable " var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> Val -> IOThrowsError Val
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ VarNotBound "Setting an unbound variable " var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return value

defineVar :: Env -> String -> Val -> IOThrowsError Val
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, Val)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
      where extendEnv binds env = liftM (++ env) (mapM addBinding binds)
            addBinding (var, value) = do
                                    ref <- newIORef value
                                    return (var, ref)
