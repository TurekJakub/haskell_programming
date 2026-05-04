module Executor where

import BlindwormParser (Ast, parseCode, tokenize)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec (errorBundlePretty)

doMain :: ([Ast] -> IO ()) -> IO ()
doMain f = do
  args <- getArgs
  case args of
    [] -> die "no input file supplied"
    (input:_) -> do
      contents <- readFile input
      case tokenize input contents of
        Right lexer_tokens ->
          case parseCode input lexer_tokens of
            Right res -> f res
            Left err -> putStrLn $ errorBundlePretty err
        Left err -> print err
