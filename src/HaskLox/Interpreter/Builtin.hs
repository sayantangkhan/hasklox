module HaskLox.Interpreter.Builtin (initializeEnvironmentWithBuiltins) where

import Data.Time.Clock.System (getSystemTime, systemSeconds)
import HaskLox.Interpreter.Environment (Environment, addIdentifier, initializeEnvironment)
import HaskLox.Interpreter.Values (BuiltinFunc (..), Value (Builtin, IntVal))

clock :: BuiltinFunc
clock = BuiltinFunc 0 "clock" (const (IntVal . fromIntegral . systemSeconds <$> getSystemTime))

initializeEnvironmentWithBuiltins :: IO (Environment Value)
initializeEnvironmentWithBuiltins = do
  environment <- initializeEnvironment
  addIdentifier "clock" (Just (Builtin clock)) environment
  return environment