module HaskLox.Interpreter.Builtin (initializeEnvironmentWithBuiltins) where

import Data.Time.Clock.System (getSystemTime, systemSeconds)
import HaskLox.Interpreter.Environment (Environment, addIdentifier, initializeEnvironment)
import HaskLox.Interpreter.Values (BuiltinFunc (..), Value (Builtin, IntVal, NilVal), prettyPrint)

initializeEnvironmentWithBuiltins :: IO (Environment Value)
initializeEnvironmentWithBuiltins = do
  environment <- initializeEnvironment
  addIdentifier "clock" (Just (Builtin clock)) environment
  addIdentifier "printfn" (Just (Builtin printFn)) environment
  return environment

clock :: BuiltinFunc
clock = BuiltinFunc 0 "clock" (const (IntVal . fromIntegral . systemSeconds <$> getSystemTime))

printFn :: BuiltinFunc
printFn = BuiltinFunc 1 "printFn" printWrapped
  where
    printWrapped a = do
      putStrLn $ prettyPrint $ head a
      return NilVal