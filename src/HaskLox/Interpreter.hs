{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskLox.Interpreter (evalExpression) where

import Control.Monad.Except
import Control.Monad.ST
import qualified Data.Text as T
import qualified HaskLox.AST as AST

data EvalError a
  = TypeError a T.Text
  | ArithmeticError a T.Text
  | UnreachableError T.Text
  deriving (Eq, Show)

data Environment = Environment
  deriving (Eq, Show)

newtype InterpreterState e s a = InterpreterState {runInterpreterState :: ExceptT e (ST s) a}
  deriving (Functor, Applicative, Monad, MonadError e)

evalExpression :: AST.Expression m -> InterpreterState (EvalError m) s (AST.Expression m)
evalExpression = \case
  AST.LiteralExp metadata literal -> do
    return $ AST.LiteralExp metadata literal
  AST.Unary _ op expression -> do
    evaled <- evalExpression expression
    applyUnaryOp op evaled
  AST.Binary metadata op left right -> undefined

applyUnaryOp :: AST.UnaryOp -> AST.Expression m -> InterpreterState (EvalError m) s (AST.Expression m)
applyUnaryOp AST.Neg = \case
  (AST.LiteralExp m literal) -> case literal of
    AST.Number m' ln -> do
      return $ AST.LiteralExp m (AST.Number m' (negateLoxNum ln))
    _ -> do
      throwError $ TypeError m "Tried to negate a non-number expression"
  _ -> do
    throwError $ UnreachableError "Should have evaluated expression to a literal"
applyUnaryOp AST.Exclamation = undefined

applyBinaryOp :: AST.BinaryOp -> m -> AST.Expression m -> AST.Expression m -> InterpreterState (EvalError m) s (AST.Expression m)
applyBinaryOp AST.IsEqual metadata = undefined
applyBinaryOp AST.NotEqual metadata = undefined
applyBinaryOp AST.Less metadata = undefined
applyBinaryOp AST.LessEqual metadata = undefined
applyBinaryOp AST.Greater metadata = undefined
applyBinaryOp AST.GreaterEqual metadata = undefined
applyBinaryOp AST.Plus metadata = undefined
applyBinaryOp AST.Minus metadata = undefined
applyBinaryOp AST.Mult metadata = undefined
applyBinaryOp AST.Divide metadata = undefined

negateLoxNum :: AST.LoxNum -> AST.LoxNum
negateLoxNum (AST.LoxInt x) = AST.LoxInt $ -1 * x
negateLoxNum (AST.LoxFloat x) = AST.LoxFloat $ -1 * x