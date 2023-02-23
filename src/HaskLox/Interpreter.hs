{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HaskLox.Interpreter (evalProgram, runInterpreter) where

import Control.Monad.Except
import Data.Text qualified as T
import HaskLox.AST qualified as AST

data EvalError a
  = TypeError a T.Text
  | ArithmeticError a T.Text
  | UnreachableError T.Text
  deriving (Eq, Show)

data Environment = Environment
  deriving (Eq, Show)

newtype InterpreterState e a = InterpreterState {runInterpreterState :: ExceptT e IO a}
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO)

runInterpreter :: InterpreterState e a -> IO (Either e a)
runInterpreter = runExceptT . runInterpreterState

evalProgram :: [AST.Statement m] -> InterpreterState (EvalError m) ()
evalProgram = mapM_ evalStatement

evalStatement :: AST.Statement m -> InterpreterState (EvalError m) ()
evalStatement = \case
  AST.ExprStmt expression -> do
    _ <- evalExpression expression
    return ()
  AST.PrintStmt expression -> do
    parsedExpression <- evalExpression expression
    liftIO $ putStrLn $ AST.ndShow parsedExpression
    return ()

evalExpression :: AST.Expression m -> InterpreterState (EvalError m) (AST.Expression m)
evalExpression = \case
  AST.LiteralExp metadata literal -> do
    return $ AST.LiteralExp metadata literal
  AST.Unary _ op expression -> do
    evaled <- evalExpression expression
    applyUnaryOp op evaled
  AST.Binary metadata op left right -> do
    leftEvaled <- evalExpression left
    rightEvaled <- evalExpression right
    applyBinaryOp op metadata leftEvaled rightEvaled

applyUnaryOp :: AST.UnaryOp -> AST.Expression m -> InterpreterState (EvalError m) (AST.Expression m)
applyUnaryOp AST.Neg = \case
  (AST.LiteralExp m literal) -> case literal of
    AST.Number m' ln -> do
      return $ AST.LiteralExp m (AST.Number m' (negateLoxNum ln))
    _ -> do
      throwError $ TypeError m "Tried to negate a non-number expression"
  _ -> do
    throwError $ UnreachableError "Should have evaluated expression to a literal"
applyUnaryOp AST.Exclamation = \case
  (AST.LiteralExp m literal) -> case literal of
    AST.LoxFalse _ -> do
      return $ AST.LiteralExp m (AST.LoxTrue m)
    AST.Nil _ -> do
      return $ AST.LiteralExp m (AST.LoxTrue m)
    _ -> do
      return $ AST.LiteralExp m (AST.LoxFalse m)
  _ -> do
    throwError $ UnreachableError "Should have evaluated expression to a literal"

applyBinaryOp :: AST.BinaryOp -> m -> AST.Expression m -> AST.Expression m -> InterpreterState (EvalError m) (AST.Expression m)
applyBinaryOp AST.IsEqual metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> do
    if leftLiteral `AST.nonMetadataEq` rightLiteral
      then return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
      else return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.NotEqual metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> do
    if leftLiteral `AST.nonMetadataEq` rightLiteral
      then return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
      else return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Less metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ x, AST.Number _ y) -> do
      if x < y
        then return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
        else return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
    _ -> do
      throwError $ TypeError metadata "Attempted to use `<` on non numeric types"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.LessEqual metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ x, AST.Number _ y) -> do
      if x <= y
        then return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
        else return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
    _ -> do
      throwError $ TypeError metadata "Attempted to use `<=` on non numeric types"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Greater metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ x, AST.Number _ y) -> do
      if x > y
        then return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
        else return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
    _ -> do
      throwError $ TypeError metadata "Attempted to use `>` on non numeric types"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.GreaterEqual metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ x, AST.Number _ y) -> do
      if x >= y
        then return $ AST.LiteralExp metadata (AST.LoxTrue metadata)
        else return $ AST.LiteralExp metadata (AST.LoxFalse metadata)
    _ -> do
      throwError $ TypeError metadata "Attempted to use `>=` on non numeric types"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Plus metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ (AST.LoxInt x), AST.Number _ (AST.LoxInt y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxInt (x + y)))
    (AST.Number _ (AST.LoxFloat x), AST.Number _ (AST.LoxFloat y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxFloat (x + y)))
    (AST.LoxString _ x, AST.LoxString _ y) -> do
      return $ AST.LiteralExp metadata (AST.LoxString metadata (x <> y))
    _ -> do
      throwError $ TypeError metadata "Attempted to use `+` on different types, or types that cannot be added"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Minus metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ (AST.LoxInt x), AST.Number _ (AST.LoxInt y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxInt (x - y)))
    (AST.Number _ (AST.LoxFloat x), AST.Number _ (AST.LoxFloat y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxFloat (x - y)))
    _ -> do
      throwError $ TypeError metadata "Attempted to use `-` on different types, or types that cannot be subtracted"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Mult metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ (AST.LoxInt x), AST.Number _ (AST.LoxInt y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxInt (x * y)))
    (AST.Number _ (AST.LoxFloat x), AST.Number _ (AST.LoxFloat y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxFloat (x * y)))
    _ -> do
      throwError $ TypeError metadata "Attempted to use `*` on different types, or types that cannot be multiplied"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"
applyBinaryOp AST.Divide metadata leftArg rightArg = case (leftArg, rightArg) of
  (AST.LiteralExp _ leftLiteral, AST.LiteralExp _ rightLiteral) -> case (leftLiteral, rightLiteral) of
    (AST.Number _ (AST.LoxInt x), AST.Number _ (AST.LoxInt y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxInt (x `div` y)))
    (AST.Number _ (AST.LoxFloat x), AST.Number _ (AST.LoxFloat y)) -> do
      return $ AST.LiteralExp metadata (AST.Number metadata (AST.LoxFloat (x / y)))
    _ -> do
      throwError $ TypeError metadata "Attempted to use `/` on different types, or types that cannot be divided"
  _ -> do
    throwError $ UnreachableError "Should have evaluated left and right arguments to literals"

negateLoxNum :: AST.LoxNum -> AST.LoxNum
negateLoxNum (AST.LoxInt x) = AST.LoxInt $ -1 * x
negateLoxNum (AST.LoxFloat x) = AST.LoxFloat $ -1 * x