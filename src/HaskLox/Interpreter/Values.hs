{-# LANGUAGE LambdaCase #-}

module HaskLox.Interpreter.Values (fromLiteral, Value (..), valueNeg, valueExcl, valueEq, valueNeq, valueLess, valueLeq, valueGreater, valueGeq, valuePlus, valueMinus, valueMult, valueDivide, prettyPrint) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)
import HaskLox.AST (Literal (..), LoxNum (..))
import HaskLox.Interpreter.Error (EvalError (..))

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
valueNeg = \case
  IntVal n -> Right $ IntVal (-n)
  FloatVal x -> Right $ FloatVal (-x)
  _ -> Left $ TypeError () "Tried to negate a non-number expression"

valueExcl :: Value -> Value
valueExcl = \case
  BoolVal False -> BoolVal True
  NilVal -> BoolVal True
  _ -> BoolVal False

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
valueLess v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ BoolVal (n1 < n2)
  (IntVal n1, FloatVal x2) -> Right $ BoolVal (fromIntegral n1 < x2)
  (FloatVal x1, IntVal n2) -> Right $ BoolVal (x1 < fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ BoolVal (x1 < x2)
  _ -> Left $ TypeError () "Attempted to use `<` on non numeric types"

valueLeq :: Value -> Value -> Either (EvalError ()) Value
valueLeq v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ BoolVal (n1 <= n2)
  (IntVal n1, FloatVal x2) -> Right $ BoolVal (fromIntegral n1 <= x2)
  (FloatVal x1, IntVal n2) -> Right $ BoolVal (x1 <= fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ BoolVal (x1 <= x2)
  _ -> Left $ TypeError () "Attempted to use `<=` on non numeric types"

valueGreater :: Value -> Value -> Either (EvalError ()) Value
valueGreater v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ BoolVal (n1 > n2)
  (IntVal n1, FloatVal x2) -> Right $ BoolVal (fromIntegral n1 > x2)
  (FloatVal x1, IntVal n2) -> Right $ BoolVal (x1 > fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ BoolVal (x1 > x2)
  _ -> Left $ TypeError () "Attempted to use `>` on non numeric types"

valueGeq :: Value -> Value -> Either (EvalError ()) Value
valueGeq v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ BoolVal (n1 >= n2)
  (IntVal n1, FloatVal x2) -> Right $ BoolVal (fromIntegral n1 >= x2)
  (FloatVal x1, IntVal n2) -> Right $ BoolVal (x1 >= fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ BoolVal (x1 >= x2)
  _ -> Left $ TypeError () "Attempted to use `>=` on non numeric types"

valuePlus :: Value -> Value -> Either (EvalError ()) Value
valuePlus v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ IntVal (n1 + n2)
  (IntVal n1, FloatVal x2) -> Right $ FloatVal (fromIntegral n1 + x2)
  (FloatVal x1, IntVal n2) -> Right $ FloatVal (x1 + fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ FloatVal (x1 + x2)
  _ -> Left $ TypeError () "Attempted to use `+` on non numeric types"

valueMinus :: Value -> Value -> Either (EvalError ()) Value
valueMinus v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ IntVal (n1 - n2)
  (IntVal n1, FloatVal x2) -> Right $ FloatVal (fromIntegral n1 - x2)
  (FloatVal x1, IntVal n2) -> Right $ FloatVal (x1 - fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ FloatVal (x1 - x2)
  _ -> Left $ TypeError () "Attempted to use `-` on non numeric types"

valueMult :: Value -> Value -> Either (EvalError ()) Value
valueMult v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ IntVal (n1 * n2)
  (IntVal n1, FloatVal x2) -> Right $ FloatVal (fromIntegral n1 * x2)
  (FloatVal x1, IntVal n2) -> Right $ FloatVal (x1 * fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ FloatVal (x1 * x2)
  _ -> Left $ TypeError () "Attempted to use `*` on non numeric types"

valueDivide :: Value -> Value -> Either (EvalError ()) Value
valueDivide v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Right $ IntVal (n1 `div` n2)
  (IntVal n1, FloatVal x2) -> Right $ FloatVal (fromIntegral n1 / x2)
  (FloatVal x1, IntVal n2) -> Right $ FloatVal (x1 / fromIntegral n2)
  (FloatVal x1, FloatVal x2) -> Right $ FloatVal (x1 / x2)
  _ -> Left $ TypeError () "Attempted to use `/` on non numeric types"
