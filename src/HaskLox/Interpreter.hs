{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module HaskLox.Interpreter (evalProgram, runInterpreter) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (forM_)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import HaskLox.AST qualified as AST
import HaskLox.Interpreter.Environment (Environment, addIdentifier, enterScope, exitScope, identifierIsPresent, lookupIdentifier, modifyIdentifier)
import HaskLox.Interpreter.Error (EvalError (..))
import HaskLox.Interpreter.Values (Value (..), fromLiteral, prettyPrint, valueDivide, valueEq, valueExcl, valueGeq, valueGreater, valueLeq, valueLess, valueMinus, valueMult, valueNeg, valueNeq, valuePlus)

newtype InterpreterState d e a = InterpreterState {runInterpreterState :: ReaderT (Environment d) (ExceptT e IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadIO,
      MonadReader (Environment d)
    )

runInterpreter :: Environment d -> InterpreterState d e a -> IO (Either e a)
runInterpreter environment interpreter = runExceptT (runReaderT (runInterpreterState interpreter) environment)

evalProgram :: AST.Program m -> InterpreterState Value (EvalError m) ()
evalProgram (AST.Program declarations) = evalDeclarations declarations

evalDeclarations :: [AST.Declaration m] -> InterpreterState Value (EvalError m) ()
evalDeclarations = mapM_ evalDeclaration

evalDeclaration :: AST.Declaration m -> InterpreterState Value (EvalError m) ()
evalDeclaration = \case
  AST.VarDeclaration _ name possibleThunk -> do
    environment <- ask
    case possibleThunk of
      Nothing -> liftIO $ addIdentifier name Nothing environment
      Just thunk -> do
        possibleValue <- evalExpression thunk
        liftIO $ addIdentifier name (Just possibleValue) environment
  AST.InnerStatement _ statement -> evalStatement statement

evalStatement :: AST.Statement m -> InterpreterState Value (EvalError m) ()
evalStatement = \case
  AST.ExprStmt _ expression -> do
    _ <- evalExpression expression
    return ()
  AST.PrintStmt _ expression -> do
    parsedExpression <- evalExpression expression
    liftIO $ putStrLn $ prettyPrint parsedExpression
    return ()
  AST.Block _ declarations -> do
    inBlock (evalDeclarations declarations)
  AST.IfStatement _ ifStatement -> do
    (truthy, _) <- isTruthy (AST.ifStatementCondition ifStatement)
    if truthy
      then do
        let thenCondition = AST.ifStatementThen ifStatement
        evalStatement thenCondition
      else do
        let elseCondition = AST.ifStatementElse ifStatement
        Data.Foldable.forM_ elseCondition evalStatement
  AST.While _ (AST.WhileStatement condition loop) -> evalWhile condition loop
  AST.For _ forStatement -> evalFor forStatement

evalWhile :: AST.Expression m -> AST.Statement m -> InterpreterState Value (EvalError m) ()
evalWhile condition loop = do
  (conditionIsTrue, _) <- isTruthy condition
  Control.Monad.when conditionIsTrue $ do
    evalStatement loop
    evalWhile condition loop

evalFor :: AST.ForStatement m -> InterpreterState Value (EvalError m) ()
evalFor (AST.ForStatement possibleInit possibleCond possibleInc body) = do
  inBlock $ do
    case possibleInit of
      Nothing -> return ()
      Just (AST.ForVarDeclr metadata name possibleValue) -> do
        let varDeclaration = AST.VarDeclaration metadata name possibleValue
        evalDeclaration varDeclaration
      Just (AST.ForInitExpression expression) -> do
        _ <- evalExpression expression
        return ()
    evalForBody possibleCond possibleInc body
  where
    evalForBody :: Maybe (AST.Expression m) -> Maybe (AST.Expression m) -> AST.Statement m -> InterpreterState Value (EvalError m) ()
    evalForBody c i b = do
      -- env <- ask
      case c of
        Nothing -> do
          inBlock $ do
            inBlock $ do
              evalStatement b
            forM_ i evalExpression
          evalForBody c i b
        Just expression -> do
          (conditionIsTrue, _) <- isTruthy expression
          when conditionIsTrue $ do
            inBlock $ do
              inBlock $ do
                evalStatement b
              forM_ i evalExpression
            evalForBody c i b

evalExpression :: AST.Expression m -> InterpreterState Value (EvalError m) Value
evalExpression = \case
  AST.LiteralExp _ literal -> do
    return $ fromLiteral literal
  AST.Unary metadata op expression -> do
    evaled <- evalExpression expression
    applyUnaryOp op metadata evaled
  AST.Binary metadata op left right -> do
    leftEvaled <- evalExpression left
    rightEvaled <- evalExpression right
    applyBinaryOp op metadata leftEvaled rightEvaled
  AST.Identifier metadata name -> do
    environment <- ask
    possibleValue <- liftIO $ lookupIdentifier name environment
    case possibleValue of
      Just (Just value) -> return value
      _ -> throwError $ ValueNotFoundError metadata ("Variable " <> (toStrict . decodeUtf8) name <> " not found in scope.")
  AST.IdentifierAssignment metadata name expression -> do
    -- We evaluate the RHS first, and then try to assign it to the left hand side
    evaledExpression <- evalExpression expression
    -- We then check if the name is actually in any scope
    environment <- ask
    isPresent <- liftIO $ identifierIsPresent name environment
    -- Depending on whether the value is present, we either re-assign it, or throw a value error.
    if isPresent
      then liftIO (modifyIdentifier name (const (Just evaledExpression)) environment) >> return evaledExpression
      else throwError $ ValueNotFoundError metadata ("Variable " <> (toStrict . decodeUtf8) name <> " not found in scope.")
  AST.LogicalAnd _ exp1 exp2 -> do
    (exp1IsTrue, exp1Evaled) <- isTruthy exp1
    if exp1IsTrue
      then evalExpression exp2
      else return exp1Evaled
  AST.LogicalOr _ exp1 exp2 -> do
    (exp1IsTrue, exp1Evaled) <- isTruthy exp1
    if exp1IsTrue
      then return exp1Evaled
      else evalExpression exp2

isTruthy :: AST.Expression m -> InterpreterState Value (EvalError m) (Bool, Value)
isTruthy expression = do
  expEvaled <- evalExpression expression
  case expEvaled of
    BoolVal False -> do
      return (False, expEvaled)
    NilVal -> do
      return (False, expEvaled)
    _ -> do
      return (True, expEvaled)

applyUnaryOp :: AST.UnaryOp -> m -> Value -> InterpreterState d (EvalError m) Value
applyUnaryOp AST.Neg metadata value = case valueNeg value of
  Right negatedValue -> return negatedValue
  Left applicationError -> throwError (metadata <$ applicationError)
applyUnaryOp AST.Exclamation _ value = return $ valueExcl value

applyBinaryOp :: AST.BinaryOp -> m -> Value -> Value -> InterpreterState d (EvalError m) Value
applyBinaryOp op metadata leftVal rightVal = case valueOp op leftVal rightVal of
  Right resultVal -> return resultVal
  Left applicationError -> throwError (metadata <$ applicationError)
  where
    valueOp :: AST.BinaryOp -> (Value -> Value -> Either (EvalError ()) Value)
    valueOp AST.IsEqual = valueEq
    valueOp AST.NotEqual = valueNeq
    valueOp AST.Less = valueLess
    valueOp AST.LessEqual = valueLeq
    valueOp AST.Greater = valueGreater
    valueOp AST.GreaterEqual = valueGeq
    valueOp AST.Plus = valuePlus
    valueOp AST.Minus = valueMinus
    valueOp AST.Mult = valueMult
    valueOp AST.Divide = valueDivide

inBlock :: InterpreterState d e a -> InterpreterState d e a
inBlock monadicAction = do
  environment <- ask
  liftIO $ enterScope environment
  resultOfAction <- monadicAction
  liftIO $ exitScope environment
  return resultOfAction
