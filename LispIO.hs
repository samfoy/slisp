module LispIO where
import Types
import Evaluate hiding (load)
import Parser
import Control.Monad.Except
import System.IO
import GHC.Base

ioPrimitives :: [(String, [Val] -> IOThrowsError Val)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [Val] -> IOThrowsError Val
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc [] = assert False undefined

makePort :: IOMode -> [Val] -> IOThrowsError Val
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = assert False undefined

closePort :: [Val] -> IOThrowsError Val
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [Val] -> IOThrowsError Val
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftT . lispParse
readProc _ = assert False undefined

writeProc :: [Val] -> IOThrowsError Val
writeProc [val] = writeProc [val, Port stdout]
writeProc [val, Port port] = liftIO $ hPrint port val >> return (Bool True)
writeProc _ = assert False undefined

readContents :: [Val] -> IOThrowsError Val
readContents [String name] = liftM String $ liftIO $ readFile name
readContents _ = assert False undefined

load :: String -> IOThrowsError [Val]
load name = liftIO (readFile name) >>= liftT . lispParseList

readAll :: [Val] -> IOThrowsError Val
readAll [String name] = liftM List $ load name
readAll _ = assert False undefined
