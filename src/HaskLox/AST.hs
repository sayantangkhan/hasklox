module HaskLox.AST (LoxNum (..), Literal (..), UnaryOp (..), BinaryOp (..), Expression (..), Statement (..), Declaration (..), HasMetadata (..), IfStatement (..), ForStatement (..), ForStatementInit (..), While (..), extendOuterMetadata, nonMetadataEq, ndShow, Program (..)) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)

newtype Program a = Program [Declaration a]
  deriving (Show)

data Declaration a
  = VarDeclaration a ByteString (Maybe (Expression a))
  | InnerStatement (Statement a)
  deriving (Show)

data Statement a
  = ExprStmt (Expression a)
  | PrintStmt (Expression a)
  | IfStatement (IfStatement a)
  | Block [Declaration a]
  | While (While a)
  | For (ForStatement a)
  deriving (Show)

data IfStatement a = IfStatementCons
  { ifStatementCondition :: Expression a,
    ifStatementThen :: Statement a,
    ifStatementElse :: Maybe (Statement a)
  }
  deriving (Show)

data While a = WhileStatement
  { whileCond :: Expression a,
    whileBody :: Statement a
  }
  deriving (Show)

data ForStatement a = ForStatement
  { fsInit :: Maybe (ForStatementInit a),
    fsCond :: Maybe (Expression a),
    fsInc :: Maybe (Expression a),
    fsBody :: Statement a
  }
  deriving (Show)

data ForStatementInit a
  = ForVarDeclr a ByteString (Maybe (Expression a))
  | ForInitExpression (Expression a)
  deriving (Show)

data Expression a
  = LiteralExp a (Literal a)
  | Unary a UnaryOp (Expression a)
  | Binary a BinaryOp (Expression a) (Expression a)
  | Identifier a ByteString
  | IdentifierAssignment a ByteString (Expression a)
  | LogicalOr a (Expression a) (Expression a)
  | LogicalAnd a (Expression a) (Expression a)

data Literal a
  = Number a LoxNum
  | LoxString a ByteString
  | LoxTrue a
  | LoxFalse a
  | Nil a

data UnaryOp
  = Neg -- -
  | Exclamation -- !
  deriving (Eq, Show)

data BinaryOp
  = IsEqual -- ==
  | NotEqual -- !=
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Plus
  | Minus
  | Mult
  | Divide
  deriving (Eq, Show)

data LoxNum
  = LoxInt Int
  | LoxFloat Float
  deriving (Eq, Show)

instance Ord LoxNum where
  LoxInt x <= LoxInt y = x <= y
  LoxFloat x <= LoxFloat y = x <= y
  LoxFloat x <= LoxInt y = x <= fromIntegral y
  LoxInt x <= LoxFloat y = fromIntegral x <= y

class HasMetadata n where
  info :: n m -> m

class NonDebugShow n where
  ndShow :: n -> String

nonMetadataEq :: Literal a -> Literal a -> Bool
nonMetadataEq (Number _ x) (Number _ y) = x == y
nonMetadataEq (LoxString _ x) (LoxString _ y) = x == y
nonMetadataEq (LoxTrue _) (LoxTrue _) = True
nonMetadataEq (LoxFalse _) (LoxFalse _) = True
nonMetadataEq (Nil _) (Nil _) = True
nonMetadataEq _ _ = False

instance (Show a) => Show (Literal a) where
  show (Number a (LoxInt n)) = show n ++ " " ++ show a
  show (Number a (LoxFloat n)) = show n ++ " " ++ show a
  show (LoxString a b) = show b ++ " " ++ show a
  show (LoxTrue a) = "true" ++ " " ++ show a
  show (LoxFalse a) = "false" ++ " " ++ show a
  show (Nil a) = "nil" ++ " " ++ show a

instance NonDebugShow (Literal a) where
  ndShow (Number _ (LoxInt n)) = show n
  ndShow (Number _ (LoxFloat n)) = show n
  ndShow (LoxString _ b) = unpack b
  ndShow (LoxTrue _) = "true"
  ndShow (LoxFalse _) = "false"
  ndShow (Nil _) = "nil"

instance HasMetadata Literal where
  info (Number a _) = a
  info (LoxString a _) = a
  info (LoxTrue a) = a
  info (LoxFalse a) = a
  info (Nil a) = a

instance NonDebugShow UnaryOp where
  ndShow = show

instance NonDebugShow BinaryOp where
  ndShow = show

-- The show instance for this datatype is defined later

prettyPrintWithOffset :: (Show a) => Expression a -> Int -> String
prettyPrintWithOffset (LiteralExp _ literal) offset =
  concat (replicate offset " ")
    ++ show literal
prettyPrintWithOffset (Unary range op expression) offset =
  concat (replicate offset " ")
    ++ show op
    ++ " "
    ++ show range
    ++ "\n"
    ++ prettyPrintWithOffset expression (offset + 2)
prettyPrintWithOffset (Binary range op exp1 exp2) offset =
  concat (replicate offset " ")
    ++ show op
    ++ " "
    ++ show range
    ++ "\n"
    ++ prettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ prettyPrintWithOffset exp2 (offset + 2)
prettyPrintWithOffset (Identifier range name) offset =
  concat (replicate offset " ")
    ++ unpack name
    ++ " "
    ++ show range
prettyPrintWithOffset (IdentifierAssignment _ name expression) offset =
  concat (replicate offset " ")
    ++ unpack name
    ++ " = \n"
    ++ prettyPrintWithOffset expression (offset + 2)
prettyPrintWithOffset (LogicalAnd range exp1 exp2) offset =
  concat
    (replicate offset " ")
    ++ "AND"
    ++ " "
    ++ show range
    ++ "\n"
    ++ prettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ prettyPrintWithOffset exp2 (offset + 2)
prettyPrintWithOffset (LogicalOr range exp1 exp2) offset =
  concat
    (replicate offset " ")
    ++ "OR"
    ++ " "
    ++ show range
    ++ "\n"
    ++ prettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ prettyPrintWithOffset exp2 (offset + 2)

ndPrettyPrintWithOffset :: Expression a -> Int -> String
ndPrettyPrintWithOffset (LiteralExp _ literal) offset =
  concat (replicate offset " ")
    ++ ndShow literal
ndPrettyPrintWithOffset (Unary _ op expression) offset =
  concat (replicate offset " ")
    ++ ndShow op
    ++ "\n"
    ++ ndPrettyPrintWithOffset expression (offset + 2)
ndPrettyPrintWithOffset (Binary _ op exp1 exp2) offset =
  concat (replicate offset " ")
    ++ ndShow op
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp2 (offset + 2)
ndPrettyPrintWithOffset (Identifier _ name) offset =
  concat (replicate offset " ")
    ++ unpack name
ndPrettyPrintWithOffset (IdentifierAssignment _ name expression) offset =
  concat (replicate offset " ")
    ++ unpack name
    ++ " = \n"
    ++ ndPrettyPrintWithOffset expression (offset + 2)
ndPrettyPrintWithOffset (LogicalAnd _ exp1 exp2) offset =
  concat
    (replicate offset " ")
    ++ "AND"
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp2 (offset + 2)
ndPrettyPrintWithOffset (LogicalOr _ exp1 exp2) offset =
  concat
    (replicate offset " ")
    ++ "OR"
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp1 (offset + 2)
    ++ "\n"
    ++ ndPrettyPrintWithOffset exp2 (offset + 2)

instance (Show a) => Show (Expression a) where
  show expression = prettyPrintWithOffset expression 0

instance NonDebugShow (Expression a) where
  ndShow expression = ndPrettyPrintWithOffset expression 0

instance HasMetadata Expression where
  info (LiteralExp a _) = a
  info (Unary a _ _) = a
  info (Binary a _ _ _) = a
  info (Identifier a _) = a
  info (IdentifierAssignment a _ _) = a
  info (LogicalAnd a _ _) = a
  info (LogicalOr a _ _) = a

-- instance HasMetadata Statement where
--   info (ExprStmt expression) = info expression
--   info (PrintStmt expression) = info expression
--   info (IfStatement ifStmt) = info $ ifStatementCondition ifStmt

extendOuterMetadata :: a -> Expression a -> Expression a
extendOuterMetadata f (LiteralExp _ b) = LiteralExp f b
extendOuterMetadata f (Unary _ b c) = Unary f b c
extendOuterMetadata f (Binary _ b c d) = Binary f b c d
extendOuterMetadata f (Identifier _ b) = Identifier f b
extendOuterMetadata f (IdentifierAssignment _ b c) = IdentifierAssignment f b c
extendOuterMetadata f (LogicalAnd _ c d) = LogicalAnd f c d
extendOuterMetadata f (LogicalOr _ c d) = LogicalOr f c d