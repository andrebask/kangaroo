module Main where

import Parser
import Syntax

import Codegen
import Emit

import Control.Monad.Trans
import System.Console.Haskeline

import System.IO
import System.Enviroment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "kangaroo is still in the marsupium"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
     Left err -> print err >> return Nothing
     Right (Block dec sts) -> do mapM_ print dec
                                 mapM_ print sts
                                 return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "k> "
    case minput of
      Nothing -> outputStrLn "Goodbye buddy."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
       []	-> repl
       [fname]	-> processFile fname >> return ()