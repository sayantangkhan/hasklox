{
module HaskLox.Scanner(
  Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  , Range (..)
  , RangedToken (..)
  , TokenType (..),
  extractInt,
  extractFloat,
  scanMany
) where

-- Documentation for how to use Alex
-- https://serokell.io/blog/lexing-with-alex

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
}

%wrapper "monadUserState-bytestring"

$spaces = [\ \t]
$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]

@identifier = ($alpha | \_) ($alpha | $digits | \_ | \')*

@string = \"[^\"]*\"


tokens :-

<0> $white+ ;

-- Keywords

<0> and {tokenBuild And}
<0> or {tokenBuild Or}
<0> else {tokenBuild Else}
<0> if {tokenBuild If}
<0> while {tokenBuild While}
<0> for {tokenBuild For}

<0> class {tokenBuild Class}
<0> fun {tokenBuild Fun}
<0> print {tokenBuild Print}
<0> return {tokenBuild Return}
<0> var {tokenBuild Var}
<0> this {tokenBuild This}
<0> nil {tokenBuild Nil}

<0> false {tokenBuild LoxFalse}
<0> true {tokenBuild LoxTrue}

-- Numbers
<0> $digits+ {tokenInt}
<0> $digits+\.$digits+ {tokenFloat}
<0> @identifier { tokenIdentifier }

-- Strings
<0> \" { beginString }
<string> \" { endString }

<string> \\n {emitSpecial "\n"}
<string> \\\" {emitSpecial "\""}
<string> \\t {emitSpecial "\t"}
<string> \\\\ {emitSpecial "\\"}
<string> \\ {illegalEscape}

<string> . {emitChar}

-- Comments
<0> \/\/ {beginComment}
<comment> \n {endComment}
<comment> . ;

<0> \/\* {nestBlockComment}
<blockcomment> \*\/ {unNestBlockComment}
<blockcomment> . ;

-- Single character tokens

<0> \( {tokenBuild LeftParen}
<0> \) {tokenBuild RightParen}
<0> \{ {tokenBuild LeftBrace}
<0> \} {tokenBuild RightBrace}
<0> \# {tokenBuild Hash}
<0> \, {tokenBuild Comma}
<0> \. {tokenBuild Dot}
<0> \- {tokenBuild Minus}
<0> \+ {tokenBuild Plus}
<0> \; {tokenBuild Semicolon}
<0> \/ {tokenBuild Slash}
<0> \* {tokenBuild Star}

<0> \! {tokenBuild Bang}
<0> \!\= {tokenBuild BangEqual}
<0> \= {tokenBuild Equal}
<0> \=\= {tokenBuild EqualEqual}
<0> \> {tokenBuild Greater}
<0> \>\= {tokenBuild GreaterEqual}
<0> \< {tokenBuild Less}
<0> \<\= {tokenBuild LessEqual}

{

data TokenType = LeftParen -- (
               | RightParen -- )
               | LeftBrace -- {
               | RightBrace -- }
               | Hash -- #
               | Comma -- ,
               | Dot -- . 
               | Minus -- -
               | Plus -- +
               | Semicolon -- ;
               | Slash -- /
               | Star -- *
               -- Infix operators
               | Bang -- !
               | BangEqual -- !=
               | Equal -- =
               | EqualEqual -- ==
               | Greater -- >
               | GreaterEqual -- >=
               | Less -- <
               | LessEqual -- <=
               -- Literals
               | Identifier ByteString
               | LoxString ByteString
               | IntNumber Int
               | FloatNumber Float
               -- Keywords
               | And
               | Class
               | Else
               | LoxFalse
               | Fun
               | For
               | If
               | Nil
               | Or
               | Print
               | Return
               | Super
               | This
               | LoxTrue
               | Var
               | While
               -- Other
               | Eof
    deriving (Eq, Show)

extractInt :: TokenType -> Maybe Int
extractInt (IntNumber i) = Just i
extractInt _ = Nothing

extractFloat :: TokenType -> Maybe Float
extractFloat (FloatNumber f) = Just f
extractFloat _ = Nothing

data Range = Range
  { rstart :: AlexPosn
  , rstop :: AlexPosn
  } deriving (Eq)

instance Show Range where
  show (Range (AlexPn _ l1 c1) (AlexPn _ l2 c2)) = "(l" ++ show l1 ++"c" ++ show c1 ++ "..l" ++ show l2 ++ "c" ++ show c2 ++ ")"

data RangedToken = RangedToken
  { rtToken :: TokenType
  , rtRange :: Range
  } deriving (Eq, Show)

data AlexUserState = AlexUserState
  {
    ausCommentNest :: Int
  , ausString :: Maybe ByteString
  , ausStringStart :: Maybe AlexPosn
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
    ausCommentNest = 0
  , ausString = Nothing
  , ausStringStart = Nothing
  }

getAus :: Alex AlexUserState
getAus = Alex $ \s -> Right (s, alex_ust s)

putAus :: AlexUserState -> Alex ()
putAus s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modifyAus :: (AlexUserState -> AlexUserState) -> Alex ()
modifyAus f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{rstart = start, rstop = stop}
  where
    stop = B.foldl' alexMove start $ B.take len str

tokenIdentifier :: AlexAction RangedToken
tokenIdentifier inp@(_, _, str, _) len =
    return RangedToken
      {
        rtToken = Identifier $ B.take len str
      , rtRange = mkRange inp len
      }

-- | WARNING: Will overflow on large integer input without warning.
tokenInt :: AlexAction RangedToken
tokenInt inp@(_, _, str, _) len = 
  return RangedToken
    {
      rtToken = IntNumber $ read $ B.unpack $ B.take len str
    , rtRange = mkRange inp len
    }

tokenFloat :: AlexAction RangedToken
tokenFloat inp@(_, _, str, _) len =
  return RangedToken
    {
      rtToken = FloatNumber $ read $ B.unpack $ B.take len str
    , rtRange = mkRange inp len
    }

tokenBuild :: TokenType -> AlexAction RangedToken
tokenBuild tokenType inp len =
  return RangedToken
    {
      rtToken = tokenType
    , rtRange = mkRange inp len
    }

beginComment :: AlexAction RangedToken
beginComment inp len = do
  alexSetStartCode comment
  skip inp len

endComment :: AlexAction RangedToken
endComment inp len = do
  alexSetStartCode 0
  skip inp len

nestBlockComment :: AlexAction RangedToken
nestBlockComment inp len = do
  alexSetStartCode blockcomment
  modifyAus $ \s -> s{ausCommentNest = ausCommentNest s + 1}
  skip inp len

unNestBlockComment :: AlexAction RangedToken
unNestBlockComment inp len = do
  nestingLevel <- ausCommentNest <$> getAus
  let newLevel = nestingLevel - 1
  modifyAus $ \s -> s{ausCommentNest = ausCommentNest s - 1}
  when (newLevel == 0) $
    alexSetStartCode 0
  skip inp len

beginString :: AlexAction RangedToken
beginString inp@(start, _, _, _) len = do
  alexSetStartCode string
  modifyAus $ \s -> s{ausString = Just ""}
  modifyAus $ \s -> s{ausStringStart = Just start}
  skip inp len

emitChar :: AlexAction RangedToken
emitChar inp@(_, _, str, _) len = do
  let toAppend = B.take len str
  modifyAus $ \s -> s{ausString = (`B.append` toAppend) <$> (ausString s)}
  skip inp len

emitSpecial :: ByteString -> AlexAction RangedToken
emitSpecial escaped inp len = do
  modifyAus $ \s -> s{ausString = (`B.append` escaped) <$> (ausString s)}
  skip inp len

illegalEscape :: AlexAction RangedToken
illegalEscape ((AlexPn _ l c), _, _, _) _ = do
  alexError $ "Illegal escape character at line " ++ (show l) ++ " and column " ++ (show c)

endString :: AlexAction RangedToken
endString inp@(start', _, str, _) len = do
  lexedString <- (fromJust . ausString) <$> getAus
  stringStart <- (fromJust . ausStringStart) <$> getAus
  modifyAus $ \s -> s{ausString = Nothing}
  modifyAus $ \s -> s{ausStringStart = Nothing}
  alexSetStartCode 0
  _ <- skip inp len
  return RangedToken
    {
      rtToken = LoxString lexedString
    , rtRange = Range {
        rstart = stringStart
      , rstop = B.foldl' alexMove start' $ B.take len str
    }
    }

alexEOF :: Alex RangedToken
alexEOF = do
    commentLevel <- ausCommentNest <$> getAus
    stringBuffer <- ausString <$> getAus
    when (commentLevel /= 0) $
      alexError "Error: unclosed block comment"
    when (isJust stringBuffer) $
      alexError "Error: unclosed string"
    (pos, _, _, _) <- alexGetInput
    return $ RangedToken Eof (Range pos pos)

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == Eof
        then pure [output]
        else (output :) <$> go

}

