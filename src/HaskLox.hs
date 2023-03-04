module HaskLox
  ( mainFunc,
    runFile,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict)
import HaskLox.AST (Expression)
import HaskLox.Environment (Environment, initializeEnvironment)
import HaskLox.Interpreter (evalProgram, runInterpreter)
import HaskLox.Parser (parseLox)
import HaskLox.REPL (runPrompt)
import HaskLox.Scanner (Range, runAlex)
import System.Environment (getArgs)
import System.Exit (exitFailure)

mainFunc :: IO ()
mainFunc = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [filename] -> runFile filename
    _ -> putStrLn "Usage: hasklox [script]" >> exitFailure

runFile :: FilePath -> IO ()
runFile filePath = do
  fileContents <- B.readFile filePath
  environment <- initializeEnvironment
  run fileContents environment

run :: B.ByteString -> Environment (Expression Range) -> IO ()
run input environment = do
  let parseResult = runAlex (fromStrict input) parseLox
  case parseResult of
    Left (_, errorMessage) -> putStrLn errorMessage
    Right parsed -> do
      runRes <- runInterpreter environment (evalProgram parsed)
      case runRes of
        Left errorMessage -> print errorMessage
        Right _ -> return ()