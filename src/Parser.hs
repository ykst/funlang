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

module Parser where
import Text.Parsec
import Text.Parsec.String 
import Control.Monad.Identity
import Control.Applicative hiding ((<|>),many,optional)
import Data.Maybe (fromJust)
import AST 

type InfixesList = [Either ScopeVar ScopeVar]
programP, bodyP, literalP :: ParsecT String InfixesList Identity AST
-- parser for literals 
literalP = lx $ choice [intLiteralP, stringLiteralP,try boolLiteralP]
  where intLiteralP    = Literal <$> (VInt . read  <$> many1 digit)
        boolLiteralP   = Literal <$> choice [VBool True <$ string "true", VBool False <$ string "false"]
        stringLiteralP = Literal <$> (VString <$> stringLiteral)
        stringLiteral  = between (string "\"") (string "\"") (many  (charLiteral <|> noneOf "\\\""))
        charLiteral    = char '\\' >> oneOf ("abfnrtv\\\"\'") >>= pure . fromJust . flip lookup escMap 
        escMap         = zip "abfnrtv\\\"\'"  "\a\b\f\n\r\t\v\\\"\'"
-- small parsers
idP    = lx $ (:) <$> letter <*> many alphaNum
infixP = lx $ many1 $ oneOf "!$%&<>?_*:;@/\\=|-+."
endP   = lx $ string "\\" >> skipMany1 (string "_") 
comment   = symbol "#" >> many (noneOf "\n")
blank     = oneOf " 　\t"
symbol s  = lx $ string s
keyword s = try $ lx $ string s <* blank
blanks    = skipMany blank
eol       = lx $ optional comment >> space 
eols      = skipMany eol
eols1     = eol >> eols
parens    = between (symbol "(") (symbol ")") 
braces    = between (symbol "{") (symbol "}") 
lx p  = p <* many blank       
--  parser for instructions (also program itself)
bodyP = List <$> endBy (spaces >> insnP) (eof<|>eols1)
  where   
  insnP   = choice [funcP,infixDefP,varP,ifP,whileP,returnP,try assignP,try dotP,try callP]
  assignP = Assign  <$> getPosition <*> try (scopeVarP  <* symbol "=") <*> expP
  funcP   = VarDef  <$> getPosition <*> (keyword "fun" >> idP) <*> funcLitP
    where 
    funcLitP = Literal <$> (VFunc <$> argListP <*> (eols1 >> bodyP <* endP) <*> pure Nothing)
  infixDefP =  do 
    pos <- getPosition
    try (string "infix")
    lr <- Left  <$ (string "L") <|> 
          Right <$ (string "R")
    prtNum <- read <$> lx (many1 digit)
    (lhs, iP, rhs) <- parens ((,,) <$> idP <*> infixP <*> idP)
    iList <- getState
    --when (elem priDef iList) $ fail "duplicate infix definition"
    b <- (eols1 >> bodyP <* endP)
    modifyState (insertAt prtNum (lr (Local iP)))
    return $ VarDef pos iP (Literal (VFunc  [lhs,rhs]  b Nothing)) 
  varP    = VarDef  <$> getPosition <*>(keyword "var" >> idP) <*> (symbol "=" >>  expP)
  whileP  = Loop    <$> getPosition <*> (keyword "while" >> expP <* eols1) <*> (bodyP <* endP )
  returnP = Return  <$> (keyword "return" >> expP)
  ifP     = Cond    <$> getPosition <*> ((keyword "if"  >> expP) <*  eols1) <*> bodyP <*> elsifP
    where elsifP = choice 
            [Nil <$ endP,
             try (symbol "else") >> space >> spaces >> bodyP <* endP,
             Cond <$> getPosition <*> (keyword "elsif" >> expP <* eols1 ) <*>  bodyP <*> elsifP] 


insertAt 0 e ls = e:ls
insertAt _ e [] = [e]
insertAt n e (l:ls) = l:insertAt (n-1) e ls
 
callP = Call <$> getPosition <*> scopeVarP <*> paramListP
constructListP = braces (sepBy expP (symbol ","))
paramListP = parens (sepBy expP (symbol ","))

argListP = parens (sepBy idP (symbol ","))
dotP =  foldl (\l (p,r) -> Dot p l r) <$> callFetchP  <*> (many1 $ symbol "." >> ((,) <$> getPosition <*> callFetchP))

scopeVarP = Outside <$> (string "@" >> idP) <|> Intrinsic <$>(string "$" >> idP)  <|> Local <$> idP
expP = getState >>= foldr lowerBP unaryP 
  where 
  lowerBP p1 p2 = case p1 of
    Left s  ->  
      foldl (\l (pos,r) -> Call pos s [l,r]) <$> p2 <*> (many $ opP s p2)
    Right s -> do 
      lft <- p2  
      (\(pos,r) -> Call pos s [lft,r]) <$> opP s (lowerBP p1 p2) <|> pure lft
  opP (Intrinsic s) p2 = (,) <$> getPosition <*> try (symbol s >> p2) 
  opP (Local s) p2 = (,) <$> getPosition <*> try (symbol s >> p2) 
  unaryP  = choice (lowestP:map lowerUP ["-","!"])
  lowerUP s = do 
    pos <- getPosition; 
    Call pos (Intrinsic ('_':s)) <$> (pure <$> (symbol s >> lowestP))
  lowestP = choice [literalP, parens expP, namelessObjP, try dotP, callFetchP] 
callFetchP = do
  pos <- getPosition
  var <- scopeVarP
  Call pos var <$> paramListP <|> Construct pos var <$> constructListP <|> pure (Fetch pos var) 
namelessObjP = Construct <$> getPosition <*> pure (Local "") <*>  constructListP     

infixes :: InfixesList
infixes = [Right (Intrinsic "++"), Left (Intrinsic "||") , Left (Intrinsic "&&") , Left (Intrinsic "<="), Left (Intrinsic "<"), Left (Intrinsic "=="), 
           Left (Intrinsic "+"), Left (Intrinsic "-"), Left (Intrinsic "/"), Left (Intrinsic "%"), Left (Intrinsic "*"),Right (Intrinsic "**")] 

programP = skipMany blank >> eols >> (try (Return <$> expP <* eof) <|> bodyP <*  eof)
fromFile p = readFile p >>= pure . runIdentity . parseString p

parseString  =  runParserT programP infixes 
