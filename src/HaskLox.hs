module HaskLox
  ( mainFunc,
  )
where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict)
import HaskLox.Interpreter (evalProgram, runInterpreter)
import HaskLox.Parser (parseLox)
import HaskLox.Scanner (runAlex)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
  ( hFlush,
    isEOF,
    stdout,
  )

mainFunc :: IO ()
mainFunc = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [filename] -> runFile filename
    _ -> putStrLn "Usage: hasklox [script]" >> exitFailure

runPrompt :: IO ()
runPrompt = do
  B.putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- B.getLine
    run line
    runPrompt

runFile :: FilePath -> IO ()
runFile filePath = do
  fileContents <- B.readFile filePath
  run fileContents

run :: B.ByteString -> IO ()
run input = do
  let parseResult = runAlex (fromStrict input) parseLox
  case parseResult of
    Left errorMessage -> putStrLn errorMessage
    Right parsed -> do
      runRes <- runInterpreter (evalProgram parsed)
      case runRes of
        Left errorMessage -> print errorMessage
        Right _ -> return ()
  return ()