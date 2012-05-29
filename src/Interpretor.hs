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
import Data.Function (on)
import Data.List
import Control.Monad.Error
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Prelude hiding (catch)
import AST
--import Env
import Parser (parseString,infixes)
import Data.IORef
import System.Process

data EvalException = 
  ERR String 
  |ENInScope String SourcePos 
  |ENOutside String SourcePos 
  |EInArg ScopeVar SourcePos Int 

data Transition = Callee | Caller deriving (Eq)

data Env = Env 
      {eax :: FVal, currentId :: Int, availableId :: [Int], heapPool :: M.Map Int HeapObject} 

data HeapObject = HeapObject 
      {varList :: [Var], parentId ::  Maybe Int} deriving (Show)

type EvalMonad = ErrorT EvalException IO

type Var = (String, FVal)

instance Show EvalException where
  show x = "Oops! " ++ showErr 
    where 
    showErr = case x of
      (ERR str) -> str
      (ENInScope fid pos) -> show fid ++ " is not in scope:" ++ show pos
      (ENOutside fid pos) -> "@ reference " ++ show fid ++ " on toplevel:" ++ show pos
      (EInArg fid pos defNum) -> 
        show fid ++ " requires " ++ show defNum ++ " arguments:" ++ show pos
  
instance Error EvalException 

instance Show Env where
  show e = unlines ["at " ++ show (currentId e) ++ ", eax = " ++ show (eax e) , 
                    M.showTree (heapPool e)]

nullEnv = Env VBot 0 [1..] (M.insert 0 (HeapObject [] Nothing) M.empty)

modEnv r mod = liftIO $ modifyIORef r mod
getEnv r = liftIO $ readIORef r 
modEax r val = liftIO $ modifyIORef r $ \env -> env {eax = val}
modHNum r i = modEnv r $ \env -> env{currentId = i}
getHeapNum r = getEnv r >>= return . currentId
getEax r = getEnv r >>= return . eax
getHeap r err i =  getEnv r >>= maybe (throwError err) return . M.lookup i . heapPool 
putEnv r env = liftIO $ writeIORef r env
getLocal r  = do 
  num <- getHeapNum r  
  hp  <- getHeap r (ERR $ "local heap missing:" ++ show num) num
  return (num,hp)
getParent r err hobj  = maybe (throwError err) (getHeap r err) (parentId hobj)

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
    where assign r (Local v) val = getLocal r >>=  \(i,hobj) -> 
             updateHeap r pos i v val hobj 
          assign r (Outside v) val = getLocal r >>= \(i,hobj) -> 
             getParent r (ENOutside v pos) hobj >>= updateHeap r pos (fromJust $ parentId hobj) v val 
  
  VarDef pos tgt eaxp -> eval r eaxp >> getEax r >>= addLocalVar r pos tgt >> return Callee
  
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
        modHNum r (scopeVal obj)
        eval r rhs >> modHNum r saveHNum >> return Callee
  
  Call pos func@(Intrinsic f) params -> do
    argVals <- pexec r [] params 
    case lookupIntrinsic f of
      Nothing -> throwError $ ENInScope f pos
      Just op -> do
        argCheck pos func (ftype op) argVals
        tCheck argVals (ftype op) pos 
        ((fdef op) r pos argVals) 
        return Callee
  
  Call pos f params -> do
    saveHNum <- getHeapNum r
    callProc r pos f params >> modHNum r saveHNum >> return Callee
  
  Construct pos f params -> do
    saveHNum <- getHeapNum r
    callProc r pos f params
    afterHNum <- getHeapNum r
    modEax r (VObj afterHNum)
    modHNum r saveHNum >> return Callee
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
            Just hid -> modHNum r hid >>  setArg r names argVals >> eval r fbody 
            Nothing -> createNewHeap r >>= modHNum r >> setArg r names argVals >> eval r fbody 
        _ -> throwError $ ERR $ show f ++ " is not a function:" ++ show pos

getVal r pos (Local v) = getLocal r >>= \(i,hobj) ->
  maybe (secondChance r pos v hobj) return  (lookup v (varList  hobj))
  where secondChance  r pos v hobj = getParent r (ENOutside v pos) hobj >>= 
          lookupHeap r pos v >>= maybe (throwError $ ENInScope v pos) return
getVal r pos (Outside v) = getLocal r >>= 
  getParent r (ENOutside v pos) . snd >>= 
  lookupHeap r pos v >>= maybe (throwError $ ENInScope v pos) return
getVal r pos (Intrinsic v) = throwError $ ERR $ "Assigining " ++ show v ++ ":" ++ show pos

pexec r vs (p:ps) = eval r p >> getEax r >>= \val -> pexec r (vs ++ [val]) ps
pexec r vs [] = return vs

update _ _ [] = []
update k1 v1 (l@(k2,_):ls) 
  | k1 == k2 = (k1,v1) : ls
  | otherwise= l : update k1 v1 ls

setArg r names argVals = getLocal r >>= \(key,hobj) -> 
  updateHeaps r key hobj {varList = zip names argVals ++ varList hobj}  

condProc pos tact fact b
  | TBool /= viewType b = throwError $ ERR $ "use boolean for condition:" ++ show pos  
  | boolVal b           = tact
  | otherwise           = fact 

addLocalVar r pos var val = getLocal r >>= \(key,hobj) -> 
  case lookup var (varList hobj) of
    Nothing -> addVarList r key var val hobj
    Just _  -> updateOne r pos key var val hobj

addVarList r key var val hobj = adjustHeaps r key $ \hobj -> 
  hobj {varList = (var,val): varList hobj}

adjustHeaps :: IORef Env -> Int -> (HeapObject -> HeapObject) -> EvalMonad () 
adjustHeaps r key hfunc = modEnv r $ \env ->
  env{heapPool = M.adjust hfunc key (heapPool env)}

createNewHeap r = do
  curNum <- getHeapNum r
  (i:is) <- return . availableId =<< getEnv r
  modEnv r $ \env -> 
    env{availableId = is, heapPool = M.insert i (HeapObject [] (Just curNum)) (heapPool env)}
  return i 

fetchWithModFunc r (VFunc args body Nothing) = 
  createNewHeap r >>= modEax r . VFunc args body . Just 
fetchWithModFunc r val = modEax r val

lookupHeap r pos v hobj =  case lookup v (varList hobj) of
  Nothing  -> getParent r (ENInScope v pos) hobj >>= lookupHeap r pos v   
  x        -> return x 

updateHeap :: IORef Env -> SourcePos -> Int -> String -> FVal -> HeapObject -> EvalMonad ()
updateHeap r pos key var val hobj = case lookup var (varList hobj) of
  Nothing -> getParent r (ENInScope var pos) hobj >>= updateHeap r pos (fromJust $ parentId hobj) var val
  Just _  -> updateOne r pos key var val hobj 
 
updateOne :: IORef Env -> SourcePos -> Int -> String -> FVal -> HeapObject -> EvalMonad ()
updateOne r pos key var val hobj = case lookup var (varList hobj) of
  Nothing -> throwError $ ENInScope var pos
  Just _  -> adjustHeaps r key $ \hobj -> hobj {varList = update var val (varList hobj)}

updateHeaps r key hobj = modEnv r $ \env -> 
  env {heapPool = M.update (const (Just hobj)) key (heapPool env)}

peaxec r vs (p:ps) = eval r p >> getEax r >>= \val -> peaxec r (vs ++ [val]) ps
peaxec r vs [] = return vs

lookupIntrinsic key = find ((==) key . fname) intrinsics
 
mdl (_,b,_) = b
data Func = Func  
  {fname :: String, ftype :: [FType], fdef :: IORef Env -> SourcePos -> [FVal] -> EvalMonad ()}
intrinsics =
   [Func "<="     [TInt,TInt]       (\r _ [x ,y] -> modEax r $ VBool $ on (<=) intVal x y),
    Func "-"     [TInt,TInt] (\r _ [x ,y] -> modEax r $ VInt $ on (-) intVal x y),
    Func "+"     [TInt,TInt] (\r _ [x ,y] -> modEax r $ VInt $ on (+) intVal x y),
    Func "*"     [TInt,TInt] (\r _ [x ,y] -> modEax r $ VInt $ on (*) intVal x y),
    Func "/"     [TInt,TInt] (\r pos [x ,y] -> if intVal y == 0 
                                 then throwError $ ERR $ "divide by zero:" ++ show pos 
                                 else modEax r $ VInt $ on div intVal x y),
    Func "%"     [TInt,TInt]       (\r _ [x ,y] -> modEax r $ VInt $ on mod intVal x y),
    Func "<"     [TInt,TInt]       (\r _ [x ,y] -> modEax r $ VBool $ on (<) intVal x y),
    Func "&&"    [TBool,TBool]     (\r _ [x ,y] -> modEax r $ VBool $ on (&&) boolVal x y),
    Func "||"    [TBool,TBool]     (\r _ [x ,y] -> modEax r $ VBool $ on (||) boolVal x y),
    Func "=="    [TAny,TAny]       (\r _ [x ,y] -> modEax r $ VBool $ x == y),
    Func "++"    [TString,TString] (\r _ [x ,y] -> modEax r $ VString $ on (++) stringVal x y),
    Func " !"    [TBool]           (\r _ [x] -> modEax r $ VBool $ not (boolVal x)),
    Func " -"    [TInt]            (\r _ [x] -> modEax r $ VInt  $ -(intVal x)),
    Func "print" [TAny](\r _ [x] -> liftIO (putStr $ show x) >> modEax r VBot),
    Func "env"   [] (\r _ [] -> getEnv r >>= liftIO . putStr . show >> modEax r VBot),
    Func "eval"  [TString] (\r pos [x] -> 
      either (\e -> throwError $ ERR $ show e ++ ":" ++ show pos) 
             (eval r) (runIdentity $ parseString "eval" (stringVal x)) >> return ()),
    Func "exec"  [TString] (\r pos [str] -> let wds = words (stringVal str) in
        unless (null wds) $ liftIO $ readProcessWithExitCode (head wds) (tail wds) "" 
        >>= putStr . mdl>> modEax r VBot),
    Func "error"   [TString] (\r _ [s] -> throwError $ERR$ stringVal s),
    Func "gc"    [] (\r _ [] -> gabageCollection r)]

gabageCollection r = modEnv r $ \env -> 
  let hp = (heapPool env)
      upw = upward hp (currentId env)
      unused = M.keys hp \\ (upw ++ mark hp 0) in 
    env {availableId = unused ++ availableId env,
         heapPool = foldl (flip M.delete) hp unused }
  where 
  upward heapList root = case parentId $ fromJust $  M.lookup root heapList of
    Just nextId -> root : upward heapList nextId 
    Nothing -> [root]
  mark heapList root = let vlist = varList (fromJust $ M.lookup root heapList) in
    root : concatMap (mark heapList) (concatMap extractRef vlist)
  extractRef (_,val) = case val of 
    VFunc _ _ (Just i) -> [i]
    VObj i -> [i]
    _ -> []

tCheck src tgt pos =  let srctp = map viewType src in
  unless (and (zipWith tComp srctp tgt)) $
    throwError $ ERR $ "expected type " ++ show tgt ++ ", but given " ++ show srctp ++ ":" ++ show pos
  where  tComp a b = a == TAny || b == TAny || a == b
 
evalMaybe :: AST -> Env -> IO (Maybe Env)
evalMaybe ast env =  do
  r <- newIORef env
  runErrorT (eval r ast) >>= either (\e -> print e >> return Nothing) (procResult r)
  where procResult r Caller = getEnv r >>= return . Just 
        procResult r _ = getEnv r >>= \env -> return $ Just env {eax = VBot}
