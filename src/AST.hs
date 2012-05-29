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

module AST where
import Text.Parsec (SourcePos)
import Data.Tree (Tree(..),drawTree)
data AST = 
  List [AST]
  |Assign    SourcePos ScopeVar  AST
  |VarDef    SourcePos String  AST 
  |Cond      SourcePos AST  AST  AST
  |Loop      SourcePos AST  AST
  |Literal   FVal
  |Call      SourcePos ScopeVar [AST] 
  |Construct SourcePos ScopeVar [AST] 
  |Dot       SourcePos AST AST
  |Fetch     SourcePos ScopeVar
  |Return    AST
  |Nil       deriving (Eq)

data FVal = 
  VInt   {intVal :: Integer} 
  |VString {stringVal :: String}
  |VBool {boolVal :: Bool} 
  |VFunc {fargs :: [String], fBody :: AST, hId ::  Maybe Int} 
  |VObj {scopeVal :: Int}
  |VBot deriving (Eq)

data ScopeVar = Local String | Outside String | Intrinsic String deriving (Eq,Show)
  
data FType = TInt | TString | TBool | TAny | TObj | TBot deriving (Eq,Show)

viewType :: FVal -> FType
viewType v = case v of
  VInt _ -> TInt
  VString _ -> TString
  VBool _ -> TBool
  VObj _ -> TObj
  _ -> TBot 

instance Show AST where
  show = drawTree . toNode

toNode :: AST -> Tree String
toNode dt = case dt of
  List asts -> Node "list" $ map toNode asts
  Assign _ s ast -> Node ("assign " ++ show s) [toNode ast]
  VarDef _ s ast -> Node ("var " ++ s ) [toNode ast]
  Cond _ c t f -> Node "cond" $ map toNode [c,t,f]
  Loop _ c b -> Node "loop" [toNode b]
  Call _ n args -> Node ("call " ++ show n) $ map toNode args
  Literal (VFunc args b _) -> Node ("funcliteral " ++ show args) [toNode b]
  Literal v -> Node ("literal " ++ show v) [] 
  Fetch _ s -> Node ("fetch " ++ show s) []
  Return ast -> Node "return" [toNode ast]
  Nil -> Node "nil" []

instance Show FVal where 
  show v = case v of
    VInt i -> show i
    VString str -> str
    VBool b
      |b ->  "true"
      |otherwise -> "false"
    VFunc _ _ v -> "function at " ++ (case v of
      Just i -> show i
      Nothing -> "?")
    VObj i -> "object at " ++ show i
    VBot -> "()"
