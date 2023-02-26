module HaskLox.REPL (runPrompt) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict)
import HaskLox.AST (Expression)
import HaskLox.Environment (Environment, initializeEnvironment)
import HaskLox.Interpreter (evalProgram, runInterpreter)
import HaskLox.Parser (parseLox)
import HaskLox.Scanner (Range, runAlex)
import System.IO

runPrompt :: IO ()
runPrompt = do
  environment <- initializeEnvironment
  runPrompt' environment

runPrompt' :: Environment (Expression Range) -> IO ()
runPrompt' environment = do
  B.putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- B.getLine
    run line environment
    runPrompt' environment

run :: B.ByteString -> Environment (Expression Range) -> IO ()
run input environment = do
  let parseResult = runAlex (fromStrict input) parseLox
  case parseResult of
    Left errorMessage -> putStrLn errorMessage
    Right parsed -> do
      runRes <- runInterpreter environment (evalProgram parsed)
      case runRes of
        Left errorMessage -> print errorMessage
        Right _ -> return ()