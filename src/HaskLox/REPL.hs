module HaskLox.REPL (runPrompt) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (forM_)
import Data.String qualified as B
import HaskLox.Interpreter (evalProgram, runInterpreter)
import HaskLox.Interpreter.Environment (Environment, initializeEnvironment)
import HaskLox.Interpreter.Values (Value)
import HaskLox.Parser (parseLox)
import HaskLox.Scanner (ausEOFfound, runAlex)
import System.Console.Haskeline qualified as HL

runPromptSingleLine :: Environment Value -> HL.InputT IO ()
runPromptSingleLine environment = do
  minput <- HL.getInputLine "> "
  case minput of
    Just line -> do
      (foundEOF, errorMessage) <- liftIO $ tryRun (B.fromString line) environment
      if foundEOF
        then runPromptMultiLine environment (B.fromString line)
        else do
          forM_ errorMessage HL.outputStrLn
          runPromptSingleLine environment
    Nothing -> return ()

runPromptMultiLine :: Environment Value -> B.ByteString -> HL.InputT IO ()
runPromptMultiLine environment prevlines = do
  minput <- HL.getInputLine ": "
  case minput of
    Just line -> runPromptMultiLine environment (prevlines `B.append` "\n" `B.append` B.fromString line)
    Nothing -> do
      (_, errorMessage) <- liftIO $ tryRun prevlines environment
      forM_ errorMessage HL.outputStrLn
      runPromptSingleLine environment

runPrompt :: IO ()
runPrompt = do
  environment <- initializeEnvironment
  HL.runInputT HL.defaultSettings $ runPromptSingleLine environment

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