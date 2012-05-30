-- Copyright (c) 2012 Yohsuke YUKISHITA, https://github.com/ykst
-- 
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
-- 
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module Interpretor where
import Control.Monad.Identity
import Text.Parsec (SourcePos)
import Data.List
import Control.Monad.Error
import Control.Monad (liftM)
import Prelude hiding (catch)
import AST
import Env
import Parser (parseString,infixes)
import Data.Function (on)
import System.Process
import Data.IORef

eval :: IORef Env -> AST -> EvalMonad Transition
eval r ast = case ast of
  Nil -> return Callee
  
  Literal val ->  modEax r val >> return Callee
  
  List (ast:asts) -> do 
    x <- eval r ast 
    if Callee /= x then return Caller else eval r (List asts)
  
  List [] -> modEax r VBot >> return Callee 
  
  Return ast -> eval r ast >> return Caller
  
  Fetch pos v -> getVal r pos v >>= fetchWithModFunc r >> return Callee
  
  Assign pos tgt eaxp -> eval r eaxp >> getEax r >>= assign r tgt >> return Callee
    where assign r (Local v) val = getLocal r >>= updateVar r pos v val
          assign r (Outside v) val = getLocal r >>= getHeap r (ENOutside v pos) . parentId  >>= updateVar r pos  v val 
  
  VarDef pos tgt eaxp -> eval r eaxp >> getEax r >>= \val -> 
    getLocal r >>= addVar r pos tgt val >> return Callee
  
  Cond pos cond tpart fpart -> eval r cond >> getEax r >>= 
    condProc pos (eval r tpart) (eval r fpart)
  
  Loop pos cond body -> eval r cond >> getEax r >>= 
    condProc pos (eval r body >> eval r ast) (return Callee)
  
  Dot pos lhs rhs -> do
    obj <- eval r lhs >> getEax r
    if viewType obj /= TObj 
      then throwError $ ERR $ show obj ++ " is not an object:" ++ show pos
      else do
        saveHNum <- getHeapNum r
        modHeapNum r (scopeVal obj)
        eval r rhs >> modHeapNum r saveHNum >> return Callee
  
  Call pos func@(Intrinsic f) params -> do
    argVals <- pexec r [] params 
    case lookupIntrinsic f of
      Nothing -> throwError $ ENInScope f pos
      Just op -> do
        argCheck pos func (ftype op) argVals
        typeCheck argVals (ftype op) pos 
        ((fdef op) r pos argVals) 
        return Callee
  
  Call pos f params -> do
    saveHNum <- getHeapNum r
    callProc r pos f params >> modHeapNum r saveHNum >> return Callee
  
  Construct pos f params -> do
    saveHNum <- getHeapNum r
    callProc r pos f params
    afterHNum <- getHeapNum r
    modEax r (VObj afterHNum)
    modHeapNum r saveHNum >> return Callee
  where
  argCheck pos f defs comes = 
    when (length defs /= length comes) $ throwError (EInArg f pos (length defs))
  callProc r pos f params = do
    argVals <- pexec r [] params
    func <- getVal r pos f
    case func of
      (VFunc names fbody ref) -> do
        argCheck pos f names argVals  
        case ref of
          Just hid -> modHeapNum r hid >>  setArg r names argVals >> eval r fbody 
          Nothing -> addHeap r >>= modHeapNum r >> setArg r names argVals >> eval r fbody 
      _ -> throwError $ ERR $ show f ++ " is not a function:" ++ show pos

pexec r vs (p:ps) = eval r p >> getEax r >>= \val -> pexec r (vs ++ [val]) ps
pexec r vs [] = return vs

setArg r names argVals = getLocal r >>= \hobj -> 
  modHeap r hobj{varList = zip names argVals ++ varList hobj}

condProc pos tact fact b
  | TBool /= viewType b = throwError $ ERR $ "non-boolean used in condition:" ++ show pos  
  | boolVal b           = tact
  | otherwise           = fact 

fetchWithModFunc r (VFunc args body Nothing) = 
  addHeap r >>= modEax r . VFunc args body . Just 
fetchWithModFunc r val = modEax r val

typeCheck src tgt pos =  let srctp = map viewType src in
  unless (and (zipWith tComp srctp tgt)) $
    throwError $ ERR $ "expected type " ++ show tgt ++ ", but given " ++ show srctp ++ ":" ++ show pos
  where  tComp a b = a == TAny || b == TAny || a == b

lookupIntrinsic key = find ((==) key . fname) intrinsics

data Func = Func  
  {fname :: String, ftype :: [FType], fdef :: IORef Env -> SourcePos -> [FVal] -> EvalMonad ()}
intrinsics =
   [Func "<="    [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VBool $ on (<=) intVal x y),
    Func "-"     [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VInt $ on (-) intVal x y),
    Func "+"     [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VInt $ on (+) intVal x y),
    Func "*"     [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VInt $ on (*) intVal x y),
    Func "/"     [TInt,TInt]        (\r pos [x ,y] -> if intVal y == 0 
                                        then throwError $ ERR $ "divide by zero:" ++ show pos 
                                        else modEax r $ VInt $ on div intVal x y),
    Func "%"     [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VInt $ on mod intVal x y),
    Func "<"     [TInt,TInt]        (\r _ [x ,y] -> modEax r $ VBool $ on (<) intVal x y),
    Func "&&"    [TBool,TBool]      (\r _ [x ,y] -> modEax r $ VBool $ on (&&) boolVal x y),
    Func "||"    [TBool,TBool]      (\r _ [x ,y] -> modEax r $ VBool $ on (||) boolVal x y),
    Func "=="    [TAny,TAny]        (\r _ [x ,y] -> modEax r $ VBool $ x == y),
    Func "++"    [TString,TString]  (\r _ [x ,y] -> modEax r $ VString $ on (++) stringVal x y),
    Func " !"    [TBool]            (\r _ [x] -> modEax r $ VBool $ not (boolVal x)),
    Func " -"    [TInt]             (\r _ [x] -> modEax r $ VInt  $ -(intVal x)),
    Func "print" [TAny]             (\r _ [x] -> liftIO (putStr $ show x) >> modEax r VBot),
    Func "env"   []                 (\r _ [] -> getEnv r >>= 
                                        liftIO . putStr . show >> modEax r VBot),
    Func "eval"  [TString]          evalImpl,
    Func "exec"  [TString]          execImpl,
    Func "error" [TString]          (\r _ [s] -> throwError $ERR$ stringVal s),
    Func "gc"    []                 (\r _ [] -> gabageCollection r)]
    where
    evalImpl r pos [x] =
          either (\e -> throwError $ ERR $ show e ++ ":" ++ show pos) 
                 (eval r) (runIdentity $ parseString "eval" (stringVal x)) >> return ()
    execImpl r pos [str] = let wds = words (stringVal str) in
            unless (null wds) $ liftIO $ readProcessWithExitCode (head wds) (tail wds) "" 
            >>= \(_,s,_) -> putStr s >> modEax r VBot
 
evalMaybe :: AST -> Env -> IO (Maybe Env)
evalMaybe ast env =  do
  r <- newIORef env
  runErrorT (eval r ast) >>= either (\e -> print e >> return Nothing) (procResult r)
  where procResult r Caller = getEnv r >>= return . Just 
        procResult r _ = getEnv r >>= \env -> return $ Just env {eax = VBot}
