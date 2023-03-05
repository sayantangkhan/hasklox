module HaskLox.REPL (runPrompt) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (forM_)
import HaskLox.Interpreter (evalProgram, runInterpreter)
import HaskLox.Interpreter.Environment (Environment, initializeEnvironment)
import HaskLox.Interpreter.Values (Value)
import HaskLox.Parser (parseLox)
import HaskLox.Scanner (ausEOFfound, runAlex)
import System.IO (hFlush, isEOF, stdout)

runPromptSingleLine :: Environment Value -> IO ()
runPromptSingleLine environment = do
  B.putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- B.getLine
    (foundEOF, errorMessage) <- tryRun line environment
    if foundEOF
      then runPromptMultiLine environment line
      else do
        forM_ errorMessage putStrLn
        runPromptSingleLine environment

runPromptMultiLine :: Environment Value -> B.ByteString -> IO ()
runPromptMultiLine environment prevLines = do
  B.putStr ": "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- B.getLine
    let lineIsNewline = line == ""
    if lineIsNewline
      then do
        (_, errorMessage) <- tryRun prevLines environment
        forM_ errorMessage putStrLn
        runPromptSingleLine environment
      else runPromptMultiLine environment (prevLines `B.append` "\n" `B.append` line)
    return ()

runPrompt :: IO ()
runPrompt = do
  environment <- initializeEnvironment
  runPromptSingleLine environment

-- tryRun tries to evaluate the input line, and returns (True, Just errorMessage) if the line failed to parse because of an early EOF, and (False, None) if it parses successfully, and (False, Just errorMessage) due to some other error
tryRun :: B.ByteString -> Environment Value -> IO (Bool, Maybe String)
tryRun input environment = do
  let parseResult = runAlex (fromStrict input) parseLox
  case parseResult of
    Left (lexerUserState, errorMessage) -> do
      if ausEOFfound lexerUserState
        then return (True, Just errorMessage)
        else do
          -- print errorMessage
          return (False, Just errorMessage)
    Right parsed -> do
      runRes <- runInterpreter environment (evalProgram parsed)
      case runRes of
        Left errorMessage -> do
          -- print errorMessage
          return (False, Just $ show errorMessage)
        Right _ -> return (False, Nothing)