module Main where

import Parser
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  -- print res
  case res of
     Left err -> print err
     Right (Block sts) -> do mapM_ print sts

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "k> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
