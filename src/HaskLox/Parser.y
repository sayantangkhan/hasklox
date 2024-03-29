{
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
module HaskLox.Parser
  ( parseLox
  ) where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified HaskLox.AST as AST
import qualified HaskLox.Scanner as Scan
}

%name parseLox program
%tokentype { Scan.RangedToken }
%error { parseError }
%monad { Scan.Alex } { >>= } { pure }
%lexer { lexer } { Scan.RangedToken Scan.Eof _ }

%token

-- Identifiers
identifier { Scan.RangedToken (Scan.Identifier _) _ }
-- Constants
string { Scan.RangedToken (Scan.LoxString _) _ }
int { Scan.RangedToken (Scan.IntNumber _) _ }
float { Scan.RangedToken (Scan.FloatNumber _) _ }
-- Keywords
and { Scan.RangedToken Scan.And _ }
class { Scan.RangedToken Scan.Class _ }
else { Scan.RangedToken Scan.Else _ }
false { Scan.RangedToken Scan.LoxFalse _ }
fun { Scan.RangedToken Scan.Fun _ }
for { Scan.RangedToken Scan.For _ }
if { Scan.RangedToken Scan.If _ }
nil { Scan.RangedToken Scan.Nil _ }
or { Scan.RangedToken Scan.Or _ }
print { Scan.RangedToken Scan.Print _ }
return { Scan.RangedToken Scan.Return _ }
super { Scan.RangedToken Scan.Super _ }
this { Scan.RangedToken Scan.This _ }
true { Scan.RangedToken Scan.LoxTrue _ }
var { Scan.RangedToken Scan.Var _ }
while { Scan.RangedToken Scan.While _ }
-- Infix operators
'!' { Scan.RangedToken Scan.Bang _ }
'!=' { Scan.RangedToken Scan.BangEqual _ }
'=' { Scan.RangedToken Scan.Equal _ }
'==' { Scan.RangedToken Scan.EqualEqual _ }
'>' { Scan.RangedToken Scan.Greater _ }
'>=' { Scan.RangedToken Scan.GreaterEqual _ }
'<' { Scan.RangedToken Scan.Less _ }
'<=' { Scan.RangedToken Scan.LessEqual _ }
'-' { Scan.RangedToken Scan.Minus _ }
'+' { Scan.RangedToken Scan.Plus _ }
'*' { Scan.RangedToken Scan.Star _ }
'/' { Scan.RangedToken Scan.Slash _ }
-- Miscellaneous
'(' { Scan.RangedToken Scan.LeftParen _ }
')' { Scan.RangedToken Scan.RightParen _ }
'{' { Scan.RangedToken Scan.LeftBrace _ }
'}' { Scan.RangedToken Scan.RightBrace _ }
'#' { Scan.RangedToken Scan.Hash _ }
',' { Scan.RangedToken Scan.Comma _ }
'.' { Scan.RangedToken Scan.Dot _ }
';' { Scan.RangedToken Scan.Semicolon _ }

%left '=' -- Assignment is the lowest precedence expression form
%left or
%left and 
%left '==' '!='
%left '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/'
%right '!' NEG
%right '('
%%

program :: { AST.Program Scan.Range }
  : reversedProgram { AST.Program $ reverse $1 }

reversedProgram :: { [AST.Declaration Scan.Range] }
  : {- empty -} { [] }
  | reversedProgram declaration { $2 : $1 }

declaration :: { AST.Declaration Scan.Range }
  : varDeclaration { $1 }
  | statement { AST.InnerStatement (AST.info $1) $1 }

varDeclaration :: { AST.Declaration Scan.Range }
  : var identifierName '=' expression ';' { AST.VarDeclaration ((Scan.rtRange $1)  <> (Scan.rtRange $5)) (snd $2) (Just $4) }
  | var identifierName ';' { AST.VarDeclaration ((Scan.rtRange $1)  <> (Scan.rtRange $3)) (snd $2) Nothing }

statement :: { AST.Statement Scan.Range }
  : openIf { $1 }
  | closedIf { $1 }

nonIfStatement :: { AST.Statement Scan.Range }
  : exprStmt { $1 }
  | printStmt { $1 }
  | block { $1 }
  -- | whileStatement { $1 }

openIf :: { AST.Statement Scan.Range }
  : if '(' expression ')' statement { AST.IfStatement (Scan.rtRange $1 <> AST.info $5) $ AST.IfStatementCons $3 $5 Nothing  }
  | if '(' expression ')' closedIf else openIf { AST.IfStatement (Scan.rtRange $1 <> AST.info $7) $ AST.IfStatementCons $3 $5 (Just $7) }
  | while '(' expression ')' openIf { AST.While (Scan.rtRange $1 <> AST.info $5) (AST.WhileStatement $3 $5) }
  | openFor { AST.For (fst $1) (snd $1) }

closedIf :: { AST.Statement Scan.Range }
  : nonIfStatement { $1 }
  | if '(' expression ')' closedIf else closedIf { AST.IfStatement (Scan.rtRange $1 <> AST.info $7) $ AST.IfStatementCons $3 $5 (Just $7) }
  | while '(' expression ')' closedIf { AST.While (Scan.rtRange $1 <> AST.info $5) (AST.WhileStatement $3 $5) }
  | closedFor { AST.For (fst $1) (snd $1) }

closedFor :: { (Scan.Range, AST.ForStatement Scan.Range) }
  : for '(' varDeclaration maybeExpression ';' maybeExpression ')' closedIf { (Scan.rtRange $1 <> AST.info $8, forConstructorVarDeclr $3 $4 $6 $8) }
  | for '(' maybeExpression ';' maybeExpression ';' maybeExpression ')' closedIf { (Scan.rtRange $1 <> AST.info $9, forConstructorExpression $3 $5 $7 $9)  }

openFor :: { (Scan.Range, AST.ForStatement Scan.Range) }
  : for '(' varDeclaration maybeExpression ';' maybeExpression ')' openIf { (Scan.rtRange $1 <> AST.info $8, forConstructorVarDeclr $3 $4 $6 $8) }
  | for '(' maybeExpression ';' maybeExpression ';' maybeExpression ')' openIf { (Scan.rtRange $1 <> AST.info $9, forConstructorExpression $3 $5 $7 $9)  }

block :: { AST.Statement Scan.Range }
  : '{' blockInner '}' { AST.Block (Scan.rtRange $1 <> Scan.rtRange $3) $2 }

blockInner :: { [AST.Declaration Scan.Range] }
  : {- empty -} { [] }
  | declaration blockInner { $1 : $2 } -- TODO: Figure out whether the reversal is necessary

printStmt :: { AST.Statement Scan.Range }
  : print expression ';' { AST.PrintStmt (Scan.rtRange $1 <> Scan.rtRange $3) $2 }

exprStmt :: { AST.Statement Scan.Range }
  : expression ';' { AST.ExprStmt (AST.info $1 <> Scan.rtRange $2) $1 }

expression :: { AST.Expression Scan.Range }
  : literal { AST.LiteralExp (AST.info $1) $1 }
  | unary { $1 }
  | binary { $1 }
  | grouping { $1 }
  | identifierName { AST.Identifier (fst $1) (snd $1) }
  | identifierAssignment { $1 }
  | expression or expression { AST.LogicalOr (AST.info $1 <> AST.info $3) $1 $3 }
  | expression and expression { AST.LogicalAnd (AST.info $1 <> AST.info $3) $1 $3 }
  | functionCall { AST.FnCallExpr (fst $1) (snd $1) }

functionCall :: { (Scan.Range, AST.FunctionCall Scan.Range) }
  : expression '(' arguments ')' { (AST.info $1 <> Scan.rtRange $4, AST.FunctionCall $1 $3) }

arguments :: { [AST.Expression Scan.Range] }
  : expression { [$1] }
  | expression ',' arguments { ($1:$3) }
  | {- empty -} { [] }

maybeExpression :: { Maybe (AST.Expression Scan.Range) }
  : expression { Just $1 }
  | {- empty -} { Nothing }

identifierName :: { (Scan.Range, ByteString) }
  : identifier { unTok $1 (\range -> \token -> (range, (fromJust $ Scan.extractIdentifier token)))}

identifierAssignment :: { AST.Expression Scan.Range }
  : identifierName '=' expression { AST.IdentifierAssignment ((fst $1) <> (AST.info $3)) (snd $1) $3 }

literal :: { AST.Literal Scan.Range }
  : int { unTok $1 (\range -> \token -> AST.Number range (AST.LoxInt (fromJust $ Scan.extractInt token))) }
  | float { unTok $1 (\range -> \token -> AST.Number range (AST.LoxFloat (fromJust $ Scan.extractFloat token))) }
  | string { unTok $1 (\range -> \token -> AST.LoxString range (fromJust $ Scan.extractString token)) }
  | true { unTok $1 (\range -> \_ -> AST.LoxTrue range) }
  | false { unTok $1 (\range -> \_ -> AST.LoxFalse range) }
  | nil { unTok $1 (\range -> \_ -> AST.Nil range) }

unary :: { AST.Expression Scan.Range }
  : '-' expression  %prec NEG{ AST.Unary ((Scan.rtRange $1) <> (AST.info $2)) AST.Neg $2 }
  | '!' expression { AST.Unary ((Scan.rtRange $1) <> (AST.info $2)) AST.Exclamation $2 }

binary :: { AST.Expression Scan.Range }
  : expression '==' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.IsEqual $1 $3 }
  | expression '!=' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.NotEqual $1 $3 }
  | expression '<' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Less $1 $3 }
  | expression '<=' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.LessEqual $1 $3 }
  | expression '>' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Greater $1 $3 }
  | expression '>=' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.GreaterEqual $1 $3 }
  | expression '+' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Plus $1 $3 }
  | expression '-' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Minus $1 $3 }
  | expression '*' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Mult $1 $3 }
  | expression '/' expression { AST.Binary ((AST.info $1) <> (AST.info $3)) AST.Divide $1 $3 }

grouping :: { AST.Expression Scan.Range }
  : '(' expression ')' { AST.extendOuterMetadata ((Scan.rtRange $1) <> (Scan.rtRange $3)) $2 }

{
parseError :: Scan.RangedToken -> Scan.Alex a
parseError token = do
  (Scan.AlexPn _ line column, _, _, _) <- Scan.alexGetInput
  Scan.alexError $ "Parse error at line " <> show line <> ", column " <> show column <> ". Found " <> show token

lexer :: (Scan.RangedToken -> Scan.Alex a) -> Scan.Alex a
lexer = (=<< Scan.alexMonadScan)

unTok :: Scan.RangedToken -> (Scan.Range -> Scan.TokenType -> a) -> a
unTok (Scan.RangedToken token range) f = f range token

-- | A kinda stupid Semigroup for Scan.Range
instance Semigroup Scan.Range where
  (<>) :: Scan.Range -> Scan.Range -> Scan.Range
  (Scan.Range a _) <> (Scan.Range _ d) = Scan.Range a d

-- Utility functions for converting between AST nodes

-- This function panics if the declaration is not a variable declaration
varDeclrToForInit :: AST.Declaration a -> AST.ForStatementInit a
varDeclrToForInit (AST.VarDeclaration m name possibleValue) = AST.ForVarDeclr m name possibleValue
varDeclrToForInit _ = error "Tried to read an inner statement when expecting a variable declaration"

forConstructorVarDeclr :: AST.Declaration a -> Maybe (AST.Expression a) -> Maybe (AST.Expression a) -> AST.Statement a -> AST.ForStatement a
forConstructorVarDeclr variableDeclaration possibleCondition possibleIncrement body = AST.ForStatement (Just (varDeclrToForInit variableDeclaration)) possibleCondition possibleIncrement body

forConstructorExpression :: Maybe (AST.Expression a) -> Maybe (AST.Expression a) -> Maybe (AST.Expression a) -> AST.Statement a -> AST.ForStatement a
forConstructorExpression possibleInitializer possibleCondition possibleIncrement body = AST.ForStatement (AST.ForInitExpression <$> possibleInitializer) possibleCondition possibleIncrement body

}
