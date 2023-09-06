{-# LANGUAGE DeriveFunctor #-}

module HaskLox.Interpreter.Error (EvalError (..)) where

import Data.Text qualified as T

data EvalError m
  = TypeError m T.Text
  | ArithmeticError m T.Text
  | ValueNotFoundError m T.Text
  | InvalidArgumentError m T.Text
  | UnreachableError T.Text
  deriving (Eq, Show, Functor)
