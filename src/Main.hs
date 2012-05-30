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

module Main where
import AST
import Env
import Interpretor
--import Env
import Parser
import System.Environment (getArgs)
import Data.List (intersperse,nub)
import System.Directory (doesFileExist)
import Control.Monad.Trans (liftIO)
import Control.Monad.Identity (runIdentity)
import Control.Monad (when)
import System.Console.Haskeline 
import System.Process
-- eaxecution
main = do
   args <- getArgs
   case args of
     [f] -> eaxecFile nullEnv f >> return ()
     [] -> runInputT defaultSettings (loop [] nullEnv)
       where 
       loop :: [String] -> Env -> InputT IO ()
       loop files env = do
         input <- getInputLine (join files ++ "> ") 
         case input of
           Nothing -> return ()
           Just command -> case words command of
             [":q"] -> return ()
             [":l",file] -> do
               fileExist <- liftIO (doesFileExist file)
               if fileExist 
                 then liftIO (eaxecFile env file) >>= maybe failProc (loop (nub $ files ++ [file]))
                 else liftIO (putStrLn ("File not found:" ++ file)) >> failProc 
             [":c"] -> liftIO (putStrLn "Environment initialized") >> loop [] nullEnv 
             ((':':_):_) -> liftIO (putStrLn commandUsage) >> loop files env
             otherwise -> 
                 liftIO (execLine env command) >>= maybe failProc (loop files)
             where failProc = liftIO (putStrLn "Environment rewinded") >> loop files env
     _ -> putStrLn "wrong arguments" 
 
commandUsage = "USAGE: quit => :q , clear => :c , load a source file => :l <file>, <program expression>"

eaxecFile :: Env -> FilePath -> IO (Maybe Env)
eaxecFile env fPath = 
  fromFile fPath >>= either (\err -> print err >> return Nothing) (evalAST env)

execLine :: Env -> String -> IO (Maybe Env)
execLine env s = case runIdentity $ parseString "Interactive" s of
  Left err -> print err >> return Nothing 
  Right ast -> evalAST env ast

silentWait = (>>= (\(_,_,_,pH) -> waitForProcess pH)) . runInteractiveCommand

evalAST env ast = evalMaybe ast env >>= 
  maybe (return Nothing) (\newEnv -> when (eax newEnv /= VBot) (print (eax newEnv)) >> return (Just newEnv))

join = concat . intersperse ","
