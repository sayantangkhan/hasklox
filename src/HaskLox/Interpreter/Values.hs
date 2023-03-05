module HaskLox.Interpreter.Values (fromLiteral, Value (..), valueNeg, valueExcl, valueEq, valueNeq, valueLess, valueLeq, valueGreater, valueGeq, valuePlus, valueMinus, valueMult, valueDivide, prettyPrint) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)
import HaskLox.AST (Literal (..), LoxNum (..))
import HaskLox.Interpreter.Error (EvalError)

data Value
  = IntVal !Int
  | FloatVal !Float
  | StringVal !ByteString
  | BoolVal !Bool
  | NilVal
  deriving (Eq, Show)

prettyPrint :: Value -> String
prettyPrint (IntVal n) = show n
prettyPrint (FloatVal x) = show x
prettyPrint (StringVal b) = unpack b
prettyPrint (BoolVal True) = "true"
prettyPrint (BoolVal False) = "false"
prettyPrint NilVal = "nil"

fromLiteral :: Literal a -> Value
fromLiteral (Number _ (LoxInt n)) = IntVal n
fromLiteral (Number _ (LoxFloat x)) = FloatVal x
fromLiteral (LoxString _ s) = StringVal s
fromLiteral (LoxTrue _) = BoolVal True
fromLiteral (LoxFalse _) = BoolVal False
fromLiteral (Nil _) = NilVal

valueNeg :: Value -> Either (EvalError ()) Value
valueNeg = undefined

valueExcl :: Value -> Value
valueExcl = undefined

valueEq :: Value -> Value -> Either (EvalError ()) Value
valueEq l r =
  if l == r
    then Right $ BoolVal True
    else Right $ BoolVal False

valueNeq :: Value -> Value -> Either (EvalError ()) Value
valueNeq l r =
  if l == r
    then Right $ BoolVal False
    else Right $ BoolVal True

valueLess :: Value -> Value -> Either (EvalError ()) Value
valueLess = undefined

valueLeq :: Value -> Value -> Either (EvalError ()) Value
valueLeq = undefined

valueGreater :: Value -> Value -> Either (EvalError ()) Value
valueGreater = undefined

valueGeq :: Value -> Value -> Either (EvalError ()) Value
valueGeq = undefined

valuePlus :: Value -> Value -> Either (EvalError ()) Value
valuePlus = undefined

valueMinus :: Value -> Value -> Either (EvalError ()) Value
valueMinus = undefined

valueMult :: Value -> Value -> Either (EvalError ()) Value
valueMult = undefined

valueDivide :: Value -> Value -> Either (EvalError ()) Value
valueDivide = undefined
