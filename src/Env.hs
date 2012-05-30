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

module Env where
import Data.List
import Data.IORef
import Control.Monad.Error
import Control.Monad (liftM)
import Text.Parsec (SourcePos)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Prelude hiding (catch)
import AST

data EvalException = 
  ERR String 
  |ENInScope String SourcePos 
  |ENOutside String SourcePos 
  |EInArg ScopeVar SourcePos Int 

data Transition = Callee | Caller deriving (Eq)

data Env = Env 
      {eax :: FVal, currentId :: Int, availableId :: [Int], heapPool :: M.Map Int HeapObject} 

data HeapObject = HeapObject 
      {varList :: [Var], thisId :: Int, parentId ::  Maybe Int} deriving (Show)

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

nullEnv = Env VBot 0 [1..] (M.insert 0 (HeapObject [] 0 Nothing) M.empty)

modEnv r mod = liftIO $ modifyIORef r mod
getEnv r = liftIO $ readIORef r 

modEax r val = liftIO $ modifyIORef r $ \env -> env {eax = val}
getEax r = getEnv r >>= return . eax

modHeapNum r i = modEnv r $ \env -> env{currentId = i}
getHeapNum r = getEnv r >>= return . currentId

modHeap r hobj = modEnv r $ \env -> env{heapPool = M.insert (thisId hobj) hobj (heapPool env)}
getHeap r err idx =  case idx of
    Nothing -> throwError err
    Just i ->  getEnv r >>= maybe (throwError err) return . M.lookup i . heapPool 

addHeap r = do
  curNum <- getHeapNum r
  (i:is) <- return . availableId =<< getEnv r
  modEnv r $ \env -> env{availableId = is, heapPool = M.insert i (HeapObject [] i (Just curNum)) (heapPool env)}
  return i 

getLocal r  = getHeapNum r >>= \num -> getHeap r (ERR $ "local heap missing:" ++ show num) (Just num)

lookupVar r pos v hobj =  case lookup v (varList hobj) of
  Nothing  -> getHeap r (ENInScope v pos) (parentId hobj) >>= lookupVar r pos v   
  x        -> return x 

updateVar :: IORef Env -> SourcePos -> String -> FVal -> HeapObject -> EvalMonad ()
updateVar r pos var val hobj = case lookup var (varList hobj) of
  Nothing -> getHeap r (ENInScope var pos) (parentId hobj) >>= updateVar r pos  var val
  Just _  -> modHeap r hobj {varList = update var val (varList hobj)} 

addVar r pos var val hobj = 
    modHeap r hobj {varList = update var val (varList hobj)} 

getVal r pos (Local v) = getLocal r >>= 
  lookupVar r pos v >>= maybe (throwError $ ENInScope v pos) return
getVal r pos (Outside v) = getLocal r >>= getHeap r (ENOutside v pos) . parentId >>= 
  lookupVar r pos v >>= maybe (throwError $ ENInScope v pos) return
getVal r pos (Intrinsic v) = throwError $ ERR $ "Assigining " ++ show v ++ ":" ++ show pos
 
update k1 v1 [] = [(k1,v1)]
update k1 v1 (l@(k2,_):ls) 
  | k1 == k2 = (k1,v1) : ls
  | otherwise= l : update k1 v1 ls


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
