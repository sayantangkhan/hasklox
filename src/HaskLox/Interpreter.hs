{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module HaskLox.Interpreter (evalProgram, runInterpreter) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (forM_)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import HaskLox.AST qualified as AST
import HaskLox.Environment (Environment, addIdentifier, enterScope, exitScope, identifierIsPresent, lookupIdentifier, modifyIdentifier)

data EvalError m
  = TypeError m T.Text
  | ArithmeticError m T.Text
  | ValueNotFoundError m T.Text
  | UnreachableError T.Text
  deriving (Eq, Show)

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

evalProgram :: AST.Program m -> InterpreterState (AST.Expression m) (EvalError m) ()
evalProgram (AST.Program declarations) = evalDeclarations declarations

evalDeclarations :: [AST.Declaration m] -> InterpreterState (AST.Expression m) (EvalError m) ()
evalDeclarations = mapM_ evalDeclaration

evalDeclaration :: AST.Declaration m -> InterpreterState (AST.Expression m) (EvalError m) ()
evalDeclaration = \case
  AST.VarDeclaration _ name possibleThunk -> do
    environment <- ask
    case possibleThunk of
      Nothing -> liftIO $ addIdentifier name Nothing environment
      Just thunk -> do
        possibleValue <- evalExpression thunk
        liftIO $ addIdentifier name (Just possibleValue) environment
  AST.InnerStatement statement -> evalStatement statement

evalStatement :: AST.Statement m -> InterpreterState (AST.Expression m) (EvalError m) ()
evalStatement = \case
  AST.ExprStmt _ expression -> do
    _ <- evalExpression expression
    return ()
  AST.PrintStmt expression -> do
    parsedExpression <- evalExpression expression
    liftIO $ putStrLn $ AST.ndShow parsedExpression
    return ()
  AST.Block declarations -> do
    inBlock (evalDeclarations declarations)
  AST.IfStatement ifStatement -> do
    -- Evaluate the condition to see if it's Truthy, i.e. not null or False
    -- TODO: Refactor this to use the isTruthy function
    evaledCondition <- evalExpression $ AST.ifStatementCondition ifStatement
    case evaledCondition of
      AST.LiteralExp _ (AST.LoxFalse _) -> do
        let elseCondition = AST.ifStatementElse ifStatement
        Data.Foldable.forM_ elseCondition evalStatement
      AST.LiteralExp _ (AST.Nil _) -> do
        let elseCondition = AST.ifStatementElse ifStatement
        Data.Foldable.forM_ elseCondition evalStatement
      _ -> do
        let thenCondition = AST.ifStatementThen ifStatement
        evalStatement thenCondition
  AST.While (AST.WhileStatement condition loop) -> evalWhile condition loop
  AST.For forStatement -> evalFor forStatement

evalWhile :: AST.Expression m -> AST.Statement m -> InterpreterState (AST.Expression m) (EvalError m) ()
evalWhile condition loop = do
  (conditionIsTrue, _) <- isTruthy condition
  Control.Monad.when conditionIsTrue $ do
    evalStatement loop
    evalWhile condition loop

evalFor :: AST.ForStatement m -> InterpreterState (AST.Expression m) (EvalError m) ()
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
    evalForBody :: Maybe (AST.Expression m) -> Maybe (AST.Expression m) -> AST.Statement m -> InterpreterState (AST.Expression m) (EvalError m) ()
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

evalExpression :: AST.Expression m -> InterpreterState (AST.Expression m) (EvalError m) (AST.Expression m)
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

isTruthy :: AST.Expression m -> InterpreterState (AST.Expression m) (EvalError m) (Bool, AST.Expression m)
isTruthy expression = do
  expEvaled <- evalExpression expression
  case expEvaled of
    AST.LiteralExp _ (AST.LoxFalse _) -> do
      return (False, expEvaled)
    AST.LiteralExp _ (AST.Nil _) -> do
      return (False, expEvaled)
    _ -> do
      return (True, expEvaled)

applyUnaryOp :: AST.UnaryOp -> AST.Expression m -> InterpreterState d (EvalError m) (AST.Expression m)
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

applyBinaryOp :: AST.BinaryOp -> m -> AST.Expression m -> AST.Expression m -> InterpreterState d (EvalError m) (AST.Expression m)
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

inBlock :: InterpreterState (AST.Expression m) (EvalError m) a -> InterpreterState (AST.Expression m) (EvalError m) a
inBlock monadicAction = do
  environment <- ask
  liftIO $ enterScope environment
  resultOfAction <- monadicAction
  liftIO $ exitScope environment
  return resultOfAction
