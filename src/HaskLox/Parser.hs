{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DeriveFoldable #-}
module HaskLox.Parser
  ( parseLox
  ) where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified HaskLox.AST as AST
import qualified HaskLox.Scanner as Scan
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Scan.RangedToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 ([AST.Declaration Scan.Range])
	| HappyAbsSyn6 (AST.Declaration Scan.Range)
	| HappyAbsSyn8 (AST.Statement Scan.Range)
	| HappyAbsSyn16 (AST.Expression Scan.Range)
	| HappyAbsSyn17 ((Scan.Range, ByteString))
	| HappyAbsSyn19 (AST.Literal Scan.Range)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Scan.RangedToken)
	-> HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80 :: () => Prelude.Int -> ({-HappyReduction (Scan.Alex) = -}
	   Prelude.Int 
	-> (Scan.RangedToken)
	-> HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> (Scan.Alex) HappyAbsSyn)
	-> [HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> (Scan.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Scan.Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48 :: () => ({-HappyReduction (Scan.Alex) = -}
	   Prelude.Int 
	-> (Scan.RangedToken)
	-> HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> (Scan.Alex) HappyAbsSyn)
	-> [HappyState (Scan.RangedToken) (HappyStk HappyAbsSyn -> (Scan.Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Scan.Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,391) ([0,0,0,0,0,0,0,0,0,51440,354,81,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,511,1,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,30720,36932,2176,0,0,0,0,0,512,0,0,0,36608,4616,272,0,18304,2308,136,0,9152,1154,68,0,37344,709,162,0,51440,354,81,0,0,0,64,0,0,65152,11,0,0,0,0,0,0,0,0,0,0,32,64,0,0,16360,32,57344,16657,8706,0,61440,8328,4353,0,30720,36932,2176,0,15360,18466,1088,0,7680,9233,544,0,36608,4616,272,0,18304,2308,136,0,9152,1154,68,0,4576,577,34,0,35056,288,17,0,17528,32912,8,0,8764,16456,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,6144,0,0,0,3840,0,0,0,1920,0,0,0,960,0,0,0,480,0,0,0,255,0,0,32768,127,0,0,59392,63,0,0,62464,95,0,0,0,0,0,17528,32912,8,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,8180,16,61440,8904,20737,0,0,0,0,0,0,1,0,0,0,0,0,0,36608,4652,1296,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseLox","program","reversedProgram","declaration","varDeclaration","statement","nonIfStatement","openIf","closedIf","block","blockInner","printStmt","exprStmt","expression","identifierName","identifierAssignment","literal","unary","binary","grouping","identifier","string","int","float","and","class","else","false","fun","for","if","nil","or","print","return","super","this","true","var","while","'!'","'!='","'='","'=='","'>'","'>='","'<'","'<='","'-'","'+'","'*'","'/'","'('","')'","'{'","'}'","'#'","','","'.'","';'","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (23) = happyShift action_20
action_2 (24) = happyShift action_21
action_2 (25) = happyShift action_22
action_2 (26) = happyShift action_23
action_2 (30) = happyShift action_24
action_2 (33) = happyShift action_25
action_2 (34) = happyShift action_26
action_2 (36) = happyShift action_27
action_2 (40) = happyShift action_28
action_2 (41) = happyShift action_29
action_2 (43) = happyShift action_30
action_2 (51) = happyShift action_31
action_2 (55) = happyShift action_32
action_2 (57) = happyShift action_33
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 (10) = happyGoto action_8
action_2 (11) = happyGoto action_9
action_2 (12) = happyGoto action_10
action_2 (14) = happyGoto action_11
action_2 (15) = happyGoto action_12
action_2 (16) = happyGoto action_13
action_2 (17) = happyGoto action_14
action_2 (18) = happyGoto action_15
action_2 (19) = happyGoto action_16
action_2 (20) = happyGoto action_17
action_2 (21) = happyGoto action_18
action_2 (22) = happyGoto action_19
action_2 _ = happyReduce_1

action_3 (63) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 _ = happyReduce_5

action_7 _ = happyReduce_15

action_8 _ = happyReduce_8

action_9 _ = happyReduce_9

action_10 _ = happyReduce_12

action_11 _ = happyReduce_11

action_12 _ = happyReduce_10

action_13 (44) = happyShift action_43
action_13 (46) = happyShift action_44
action_13 (47) = happyShift action_45
action_13 (48) = happyShift action_46
action_13 (49) = happyShift action_47
action_13 (50) = happyShift action_48
action_13 (51) = happyShift action_49
action_13 (52) = happyShift action_50
action_13 (53) = happyShift action_51
action_13 (54) = happyShift action_52
action_13 (62) = happyShift action_53
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (45) = happyShift action_42
action_14 _ = happyReduce_26

action_15 _ = happyReduce_27

action_16 _ = happyReduce_22

action_17 _ = happyReduce_23

action_18 _ = happyReduce_24

action_19 _ = happyReduce_25

action_20 _ = happyReduce_28

action_21 _ = happyReduce_32

action_22 _ = happyReduce_30

action_23 _ = happyReduce_31

action_24 _ = happyReduce_34

action_25 (55) = happyShift action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_35

action_27 (23) = happyShift action_20
action_27 (24) = happyShift action_21
action_27 (25) = happyShift action_22
action_27 (26) = happyShift action_23
action_27 (30) = happyShift action_24
action_27 (34) = happyShift action_26
action_27 (40) = happyShift action_28
action_27 (43) = happyShift action_30
action_27 (51) = happyShift action_31
action_27 (55) = happyShift action_32
action_27 (16) = happyGoto action_40
action_27 (17) = happyGoto action_14
action_27 (18) = happyGoto action_15
action_27 (19) = happyGoto action_16
action_27 (20) = happyGoto action_17
action_27 (21) = happyGoto action_18
action_27 (22) = happyGoto action_19
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_33

action_29 (23) = happyShift action_20
action_29 (17) = happyGoto action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (23) = happyShift action_20
action_30 (24) = happyShift action_21
action_30 (25) = happyShift action_22
action_30 (26) = happyShift action_23
action_30 (30) = happyShift action_24
action_30 (34) = happyShift action_26
action_30 (40) = happyShift action_28
action_30 (43) = happyShift action_30
action_30 (51) = happyShift action_31
action_30 (55) = happyShift action_32
action_30 (16) = happyGoto action_38
action_30 (17) = happyGoto action_14
action_30 (18) = happyGoto action_15
action_30 (19) = happyGoto action_16
action_30 (20) = happyGoto action_17
action_30 (21) = happyGoto action_18
action_30 (22) = happyGoto action_19
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (23) = happyShift action_20
action_31 (24) = happyShift action_21
action_31 (25) = happyShift action_22
action_31 (26) = happyShift action_23
action_31 (30) = happyShift action_24
action_31 (34) = happyShift action_26
action_31 (40) = happyShift action_28
action_31 (43) = happyShift action_30
action_31 (51) = happyShift action_31
action_31 (55) = happyShift action_32
action_31 (16) = happyGoto action_37
action_31 (17) = happyGoto action_14
action_31 (18) = happyGoto action_15
action_31 (19) = happyGoto action_16
action_31 (20) = happyGoto action_17
action_31 (21) = happyGoto action_18
action_31 (22) = happyGoto action_19
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (23) = happyShift action_20
action_32 (24) = happyShift action_21
action_32 (25) = happyShift action_22
action_32 (26) = happyShift action_23
action_32 (30) = happyShift action_24
action_32 (34) = happyShift action_26
action_32 (40) = happyShift action_28
action_32 (43) = happyShift action_30
action_32 (51) = happyShift action_31
action_32 (55) = happyShift action_32
action_32 (16) = happyGoto action_36
action_32 (17) = happyGoto action_14
action_32 (18) = happyGoto action_15
action_32 (19) = happyGoto action_16
action_32 (20) = happyGoto action_17
action_32 (21) = happyGoto action_18
action_32 (22) = happyGoto action_19
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (23) = happyShift action_20
action_33 (24) = happyShift action_21
action_33 (25) = happyShift action_22
action_33 (26) = happyShift action_23
action_33 (30) = happyShift action_24
action_33 (33) = happyShift action_25
action_33 (34) = happyShift action_26
action_33 (36) = happyShift action_27
action_33 (40) = happyShift action_28
action_33 (41) = happyShift action_29
action_33 (43) = happyShift action_30
action_33 (51) = happyShift action_31
action_33 (55) = happyShift action_32
action_33 (57) = happyShift action_33
action_33 (6) = happyGoto action_34
action_33 (7) = happyGoto action_5
action_33 (8) = happyGoto action_6
action_33 (9) = happyGoto action_7
action_33 (10) = happyGoto action_8
action_33 (11) = happyGoto action_9
action_33 (12) = happyGoto action_10
action_33 (13) = happyGoto action_35
action_33 (14) = happyGoto action_11
action_33 (15) = happyGoto action_12
action_33 (16) = happyGoto action_13
action_33 (17) = happyGoto action_14
action_33 (18) = happyGoto action_15
action_33 (19) = happyGoto action_16
action_33 (20) = happyGoto action_17
action_33 (21) = happyGoto action_18
action_33 (22) = happyGoto action_19
action_33 _ = happyReduce_18

action_34 (23) = happyShift action_20
action_34 (24) = happyShift action_21
action_34 (25) = happyShift action_22
action_34 (26) = happyShift action_23
action_34 (30) = happyShift action_24
action_34 (33) = happyShift action_25
action_34 (34) = happyShift action_26
action_34 (36) = happyShift action_27
action_34 (40) = happyShift action_28
action_34 (41) = happyShift action_29
action_34 (43) = happyShift action_30
action_34 (51) = happyShift action_31
action_34 (55) = happyShift action_32
action_34 (57) = happyShift action_33
action_34 (6) = happyGoto action_34
action_34 (7) = happyGoto action_5
action_34 (8) = happyGoto action_6
action_34 (9) = happyGoto action_7
action_34 (10) = happyGoto action_8
action_34 (11) = happyGoto action_9
action_34 (12) = happyGoto action_10
action_34 (13) = happyGoto action_71
action_34 (14) = happyGoto action_11
action_34 (15) = happyGoto action_12
action_34 (16) = happyGoto action_13
action_34 (17) = happyGoto action_14
action_34 (18) = happyGoto action_15
action_34 (19) = happyGoto action_16
action_34 (20) = happyGoto action_17
action_34 (21) = happyGoto action_18
action_34 (22) = happyGoto action_19
action_34 _ = happyReduce_18

action_35 (58) = happyShift action_70
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (44) = happyShift action_43
action_36 (46) = happyShift action_44
action_36 (47) = happyShift action_45
action_36 (48) = happyShift action_46
action_36 (49) = happyShift action_47
action_36 (50) = happyShift action_48
action_36 (51) = happyShift action_49
action_36 (52) = happyShift action_50
action_36 (53) = happyShift action_51
action_36 (54) = happyShift action_52
action_36 (56) = happyShift action_69
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_36

action_38 _ = happyReduce_37

action_39 (45) = happyShift action_67
action_39 (62) = happyShift action_68
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (44) = happyShift action_43
action_40 (46) = happyShift action_44
action_40 (47) = happyShift action_45
action_40 (48) = happyShift action_46
action_40 (49) = happyShift action_47
action_40 (50) = happyShift action_48
action_40 (51) = happyShift action_49
action_40 (52) = happyShift action_50
action_40 (53) = happyShift action_51
action_40 (54) = happyShift action_52
action_40 (62) = happyShift action_66
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (23) = happyShift action_20
action_41 (24) = happyShift action_21
action_41 (25) = happyShift action_22
action_41 (26) = happyShift action_23
action_41 (30) = happyShift action_24
action_41 (34) = happyShift action_26
action_41 (40) = happyShift action_28
action_41 (43) = happyShift action_30
action_41 (51) = happyShift action_31
action_41 (55) = happyShift action_32
action_41 (16) = happyGoto action_65
action_41 (17) = happyGoto action_14
action_41 (18) = happyGoto action_15
action_41 (19) = happyGoto action_16
action_41 (20) = happyGoto action_17
action_41 (21) = happyGoto action_18
action_41 (22) = happyGoto action_19
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (23) = happyShift action_20
action_42 (24) = happyShift action_21
action_42 (25) = happyShift action_22
action_42 (26) = happyShift action_23
action_42 (30) = happyShift action_24
action_42 (34) = happyShift action_26
action_42 (40) = happyShift action_28
action_42 (43) = happyShift action_30
action_42 (51) = happyShift action_31
action_42 (55) = happyShift action_32
action_42 (16) = happyGoto action_64
action_42 (17) = happyGoto action_14
action_42 (18) = happyGoto action_15
action_42 (19) = happyGoto action_16
action_42 (20) = happyGoto action_17
action_42 (21) = happyGoto action_18
action_42 (22) = happyGoto action_19
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (23) = happyShift action_20
action_43 (24) = happyShift action_21
action_43 (25) = happyShift action_22
action_43 (26) = happyShift action_23
action_43 (30) = happyShift action_24
action_43 (34) = happyShift action_26
action_43 (40) = happyShift action_28
action_43 (43) = happyShift action_30
action_43 (51) = happyShift action_31
action_43 (55) = happyShift action_32
action_43 (16) = happyGoto action_63
action_43 (17) = happyGoto action_14
action_43 (18) = happyGoto action_15
action_43 (19) = happyGoto action_16
action_43 (20) = happyGoto action_17
action_43 (21) = happyGoto action_18
action_43 (22) = happyGoto action_19
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (23) = happyShift action_20
action_44 (24) = happyShift action_21
action_44 (25) = happyShift action_22
action_44 (26) = happyShift action_23
action_44 (30) = happyShift action_24
action_44 (34) = happyShift action_26
action_44 (40) = happyShift action_28
action_44 (43) = happyShift action_30
action_44 (51) = happyShift action_31
action_44 (55) = happyShift action_32
action_44 (16) = happyGoto action_62
action_44 (17) = happyGoto action_14
action_44 (18) = happyGoto action_15
action_44 (19) = happyGoto action_16
action_44 (20) = happyGoto action_17
action_44 (21) = happyGoto action_18
action_44 (22) = happyGoto action_19
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (23) = happyShift action_20
action_45 (24) = happyShift action_21
action_45 (25) = happyShift action_22
action_45 (26) = happyShift action_23
action_45 (30) = happyShift action_24
action_45 (34) = happyShift action_26
action_45 (40) = happyShift action_28
action_45 (43) = happyShift action_30
action_45 (51) = happyShift action_31
action_45 (55) = happyShift action_32
action_45 (16) = happyGoto action_61
action_45 (17) = happyGoto action_14
action_45 (18) = happyGoto action_15
action_45 (19) = happyGoto action_16
action_45 (20) = happyGoto action_17
action_45 (21) = happyGoto action_18
action_45 (22) = happyGoto action_19
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (23) = happyShift action_20
action_46 (24) = happyShift action_21
action_46 (25) = happyShift action_22
action_46 (26) = happyShift action_23
action_46 (30) = happyShift action_24
action_46 (34) = happyShift action_26
action_46 (40) = happyShift action_28
action_46 (43) = happyShift action_30
action_46 (51) = happyShift action_31
action_46 (55) = happyShift action_32
action_46 (16) = happyGoto action_60
action_46 (17) = happyGoto action_14
action_46 (18) = happyGoto action_15
action_46 (19) = happyGoto action_16
action_46 (20) = happyGoto action_17
action_46 (21) = happyGoto action_18
action_46 (22) = happyGoto action_19
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (23) = happyShift action_20
action_47 (24) = happyShift action_21
action_47 (25) = happyShift action_22
action_47 (26) = happyShift action_23
action_47 (30) = happyShift action_24
action_47 (34) = happyShift action_26
action_47 (40) = happyShift action_28
action_47 (43) = happyShift action_30
action_47 (51) = happyShift action_31
action_47 (55) = happyShift action_32
action_47 (16) = happyGoto action_59
action_47 (17) = happyGoto action_14
action_47 (18) = happyGoto action_15
action_47 (19) = happyGoto action_16
action_47 (20) = happyGoto action_17
action_47 (21) = happyGoto action_18
action_47 (22) = happyGoto action_19
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (23) = happyShift action_20
action_48 (24) = happyShift action_21
action_48 (25) = happyShift action_22
action_48 (26) = happyShift action_23
action_48 (30) = happyShift action_24
action_48 (34) = happyShift action_26
action_48 (40) = happyShift action_28
action_48 (43) = happyShift action_30
action_48 (51) = happyShift action_31
action_48 (55) = happyShift action_32
action_48 (16) = happyGoto action_58
action_48 (17) = happyGoto action_14
action_48 (18) = happyGoto action_15
action_48 (19) = happyGoto action_16
action_48 (20) = happyGoto action_17
action_48 (21) = happyGoto action_18
action_48 (22) = happyGoto action_19
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (23) = happyShift action_20
action_49 (24) = happyShift action_21
action_49 (25) = happyShift action_22
action_49 (26) = happyShift action_23
action_49 (30) = happyShift action_24
action_49 (34) = happyShift action_26
action_49 (40) = happyShift action_28
action_49 (43) = happyShift action_30
action_49 (51) = happyShift action_31
action_49 (55) = happyShift action_32
action_49 (16) = happyGoto action_57
action_49 (17) = happyGoto action_14
action_49 (18) = happyGoto action_15
action_49 (19) = happyGoto action_16
action_49 (20) = happyGoto action_17
action_49 (21) = happyGoto action_18
action_49 (22) = happyGoto action_19
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (23) = happyShift action_20
action_50 (24) = happyShift action_21
action_50 (25) = happyShift action_22
action_50 (26) = happyShift action_23
action_50 (30) = happyShift action_24
action_50 (34) = happyShift action_26
action_50 (40) = happyShift action_28
action_50 (43) = happyShift action_30
action_50 (51) = happyShift action_31
action_50 (55) = happyShift action_32
action_50 (16) = happyGoto action_56
action_50 (17) = happyGoto action_14
action_50 (18) = happyGoto action_15
action_50 (19) = happyGoto action_16
action_50 (20) = happyGoto action_17
action_50 (21) = happyGoto action_18
action_50 (22) = happyGoto action_19
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (23) = happyShift action_20
action_51 (24) = happyShift action_21
action_51 (25) = happyShift action_22
action_51 (26) = happyShift action_23
action_51 (30) = happyShift action_24
action_51 (34) = happyShift action_26
action_51 (40) = happyShift action_28
action_51 (43) = happyShift action_30
action_51 (51) = happyShift action_31
action_51 (55) = happyShift action_32
action_51 (16) = happyGoto action_55
action_51 (17) = happyGoto action_14
action_51 (18) = happyGoto action_15
action_51 (19) = happyGoto action_16
action_51 (20) = happyGoto action_17
action_51 (21) = happyGoto action_18
action_51 (22) = happyGoto action_19
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (23) = happyShift action_20
action_52 (24) = happyShift action_21
action_52 (25) = happyShift action_22
action_52 (26) = happyShift action_23
action_52 (30) = happyShift action_24
action_52 (34) = happyShift action_26
action_52 (40) = happyShift action_28
action_52 (43) = happyShift action_30
action_52 (51) = happyShift action_31
action_52 (55) = happyShift action_32
action_52 (16) = happyGoto action_54
action_52 (17) = happyGoto action_14
action_52 (18) = happyGoto action_15
action_52 (19) = happyGoto action_16
action_52 (20) = happyGoto action_17
action_52 (21) = happyGoto action_18
action_52 (22) = happyGoto action_19
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_21

action_54 _ = happyReduce_47

action_55 _ = happyReduce_46

action_56 (53) = happyShift action_51
action_56 (54) = happyShift action_52
action_56 _ = happyReduce_44

action_57 (53) = happyShift action_51
action_57 (54) = happyShift action_52
action_57 _ = happyReduce_45

action_58 (51) = happyShift action_49
action_58 (52) = happyShift action_50
action_58 (53) = happyShift action_51
action_58 (54) = happyShift action_52
action_58 _ = happyReduce_41

action_59 (51) = happyShift action_49
action_59 (52) = happyShift action_50
action_59 (53) = happyShift action_51
action_59 (54) = happyShift action_52
action_59 _ = happyReduce_40

action_60 (51) = happyShift action_49
action_60 (52) = happyShift action_50
action_60 (53) = happyShift action_51
action_60 (54) = happyShift action_52
action_60 _ = happyReduce_43

action_61 (51) = happyShift action_49
action_61 (52) = happyShift action_50
action_61 (53) = happyShift action_51
action_61 (54) = happyShift action_52
action_61 _ = happyReduce_42

action_62 (47) = happyShift action_45
action_62 (48) = happyShift action_46
action_62 (49) = happyShift action_47
action_62 (50) = happyShift action_48
action_62 (51) = happyShift action_49
action_62 (52) = happyShift action_50
action_62 (53) = happyShift action_51
action_62 (54) = happyShift action_52
action_62 _ = happyReduce_38

action_63 (47) = happyShift action_45
action_63 (48) = happyShift action_46
action_63 (49) = happyShift action_47
action_63 (50) = happyShift action_48
action_63 (51) = happyShift action_49
action_63 (52) = happyShift action_50
action_63 (53) = happyShift action_51
action_63 (54) = happyShift action_52
action_63 _ = happyReduce_39

action_64 (44) = happyShift action_43
action_64 (46) = happyShift action_44
action_64 (47) = happyShift action_45
action_64 (48) = happyShift action_46
action_64 (49) = happyShift action_47
action_64 (50) = happyShift action_48
action_64 (51) = happyShift action_49
action_64 (52) = happyShift action_50
action_64 (53) = happyShift action_51
action_64 (54) = happyShift action_52
action_64 _ = happyReduce_29

action_65 (44) = happyShift action_43
action_65 (46) = happyShift action_44
action_65 (47) = happyShift action_45
action_65 (48) = happyShift action_46
action_65 (49) = happyShift action_47
action_65 (50) = happyShift action_48
action_65 (51) = happyShift action_49
action_65 (52) = happyShift action_50
action_65 (53) = happyShift action_51
action_65 (54) = happyShift action_52
action_65 (56) = happyShift action_74
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_20

action_67 (23) = happyShift action_20
action_67 (24) = happyShift action_21
action_67 (25) = happyShift action_22
action_67 (26) = happyShift action_23
action_67 (30) = happyShift action_24
action_67 (34) = happyShift action_26
action_67 (40) = happyShift action_28
action_67 (43) = happyShift action_30
action_67 (51) = happyShift action_31
action_67 (55) = happyShift action_32
action_67 (16) = happyGoto action_73
action_67 (17) = happyGoto action_14
action_67 (18) = happyGoto action_15
action_67 (19) = happyGoto action_16
action_67 (20) = happyGoto action_17
action_67 (21) = happyGoto action_18
action_67 (22) = happyGoto action_19
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_7

action_69 _ = happyReduce_48

action_70 (62) = happyShift action_72
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_19

action_72 _ = happyReduce_17

action_73 (44) = happyShift action_43
action_73 (46) = happyShift action_44
action_73 (47) = happyShift action_45
action_73 (48) = happyShift action_46
action_73 (49) = happyShift action_47
action_73 (50) = happyShift action_48
action_73 (51) = happyShift action_49
action_73 (52) = happyShift action_50
action_73 (53) = happyShift action_51
action_73 (54) = happyShift action_52
action_73 (62) = happyShift action_77
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (23) = happyShift action_20
action_74 (24) = happyShift action_21
action_74 (25) = happyShift action_22
action_74 (26) = happyShift action_23
action_74 (30) = happyShift action_24
action_74 (33) = happyShift action_25
action_74 (34) = happyShift action_26
action_74 (36) = happyShift action_27
action_74 (40) = happyShift action_28
action_74 (43) = happyShift action_30
action_74 (51) = happyShift action_31
action_74 (55) = happyShift action_32
action_74 (57) = happyShift action_33
action_74 (8) = happyGoto action_75
action_74 (9) = happyGoto action_7
action_74 (10) = happyGoto action_8
action_74 (11) = happyGoto action_76
action_74 (12) = happyGoto action_10
action_74 (14) = happyGoto action_11
action_74 (15) = happyGoto action_12
action_74 (16) = happyGoto action_13
action_74 (17) = happyGoto action_14
action_74 (18) = happyGoto action_15
action_74 (19) = happyGoto action_16
action_74 (20) = happyGoto action_17
action_74 (21) = happyGoto action_18
action_74 (22) = happyGoto action_19
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_13

action_76 (29) = happyShift action_78
action_76 _ = happyReduce_9

action_77 _ = happyReduce_6

action_78 (23) = happyShift action_20
action_78 (24) = happyShift action_21
action_78 (25) = happyShift action_22
action_78 (26) = happyShift action_23
action_78 (30) = happyShift action_24
action_78 (33) = happyShift action_25
action_78 (34) = happyShift action_26
action_78 (36) = happyShift action_27
action_78 (40) = happyShift action_28
action_78 (43) = happyShift action_30
action_78 (51) = happyShift action_31
action_78 (55) = happyShift action_32
action_78 (57) = happyShift action_33
action_78 (9) = happyGoto action_7
action_78 (10) = happyGoto action_79
action_78 (11) = happyGoto action_80
action_78 (12) = happyGoto action_10
action_78 (14) = happyGoto action_11
action_78 (15) = happyGoto action_12
action_78 (16) = happyGoto action_13
action_78 (17) = happyGoto action_14
action_78 (18) = happyGoto action_15
action_78 (19) = happyGoto action_16
action_78 (20) = happyGoto action_17
action_78 (21) = happyGoto action_18
action_78 (22) = happyGoto action_19
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_14

action_80 _ = happyReduce_16

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (AST.InnerStatement happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.VarDeclaration ((Scan.rtRange happy_var_1)  <-> (AST.info happy_var_4)) (snd happy_var_2) (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (AST.VarDeclaration ((Scan.rtRange happy_var_1)  <-> (fst happy_var_2)) (snd happy_var_2) Nothing
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.IfStatementCons happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 7 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.IfStatementCons happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 7 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.IfStatementCons happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 12 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.Block happy_var_2
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_0  13 happyReduction_18
happyReduction_18  =  HappyAbsSyn4
		 ([]
	)

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AST.PrintStmt happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  15 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn8
		 (AST.ExprStmt happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.LiteralExp (AST.info happy_var_1) happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Identifier (fst happy_var_1) (snd happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (unTok happy_var_1 (\range -> \token -> (range, (fromJust $ Scan.extractIdentifier token)))
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.IdentifierAssignment ((fst happy_var_1) <-> (AST.info happy_var_3)) (snd happy_var_1) happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \token -> AST.Number range (AST.LoxInt (fromJust $ Scan.extractInt token)))
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \token -> AST.Number range (AST.LoxFloat (fromJust $ Scan.extractFloat token)))
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \token -> AST.LoxString range (fromJust $ Scan.extractString token))
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  19 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \_ -> AST.LoxTrue range)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \_ -> AST.LoxFalse range)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (unTok happy_var_1 (\range -> \_ -> AST.Nil range)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  20 happyReduction_36
happyReduction_36 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Unary ((Scan.rtRange happy_var_1) <-> (AST.info happy_var_2)) AST.Neg happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  20 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Unary ((Scan.rtRange happy_var_1) <-> (AST.info happy_var_2)) AST.Exclamation happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  21 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.IsEqual happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  21 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.NotEqual happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Less happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.LessEqual happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Greater happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Plus happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Minus happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  21 happyReduction_46
happyReduction_46 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Mult happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  21 happyReduction_47
happyReduction_47 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (AST.Binary ((AST.info happy_var_1) <-> (AST.info happy_var_3)) AST.Divide happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (AST.extendOuterMetadata ((Scan.rtRange happy_var_1) <-> (Scan.rtRange happy_var_3)) happy_var_2
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Scan.RangedToken Scan.Eof _ -> action 63 63 tk (HappyState action) sts stk;
	Scan.RangedToken (Scan.Identifier _) _ -> cont 23;
	Scan.RangedToken (Scan.LoxString _) _ -> cont 24;
	Scan.RangedToken (Scan.IntNumber _) _ -> cont 25;
	Scan.RangedToken (Scan.FloatNumber _) _ -> cont 26;
	Scan.RangedToken Scan.And _ -> cont 27;
	Scan.RangedToken Scan.Class _ -> cont 28;
	Scan.RangedToken Scan.Else _ -> cont 29;
	Scan.RangedToken Scan.LoxFalse _ -> cont 30;
	Scan.RangedToken Scan.Fun _ -> cont 31;
	Scan.RangedToken Scan.For _ -> cont 32;
	Scan.RangedToken Scan.If _ -> cont 33;
	Scan.RangedToken Scan.Nil _ -> cont 34;
	Scan.RangedToken Scan.Or _ -> cont 35;
	Scan.RangedToken Scan.Print _ -> cont 36;
	Scan.RangedToken Scan.Return _ -> cont 37;
	Scan.RangedToken Scan.Super _ -> cont 38;
	Scan.RangedToken Scan.This _ -> cont 39;
	Scan.RangedToken Scan.LoxTrue _ -> cont 40;
	Scan.RangedToken Scan.Var _ -> cont 41;
	Scan.RangedToken Scan.While _ -> cont 42;
	Scan.RangedToken Scan.Bang _ -> cont 43;
	Scan.RangedToken Scan.BangEqual _ -> cont 44;
	Scan.RangedToken Scan.Equal _ -> cont 45;
	Scan.RangedToken Scan.EqualEqual _ -> cont 46;
	Scan.RangedToken Scan.Greater _ -> cont 47;
	Scan.RangedToken Scan.GreaterEqual _ -> cont 48;
	Scan.RangedToken Scan.Less _ -> cont 49;
	Scan.RangedToken Scan.LessEqual _ -> cont 50;
	Scan.RangedToken Scan.Minus _ -> cont 51;
	Scan.RangedToken Scan.Plus _ -> cont 52;
	Scan.RangedToken Scan.Star _ -> cont 53;
	Scan.RangedToken Scan.Slash _ -> cont 54;
	Scan.RangedToken Scan.LeftParen _ -> cont 55;
	Scan.RangedToken Scan.RightParen _ -> cont 56;
	Scan.RangedToken Scan.LeftBrace _ -> cont 57;
	Scan.RangedToken Scan.RightBrace _ -> cont 58;
	Scan.RangedToken Scan.Hash _ -> cont 59;
	Scan.RangedToken Scan.Comma _ -> cont 60;
	Scan.RangedToken Scan.Dot _ -> cont 61;
	Scan.RangedToken Scan.Semicolon _ -> cont 62;
	_ -> happyError' (tk, [])
	})

happyError_ explist 63 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Scan.Alex a -> (a -> Scan.Alex b) -> Scan.Alex b
happyThen = (>>=)
happyReturn :: () => a -> Scan.Alex a
happyReturn = (pure)
happyThen1 :: () => Scan.Alex a -> (a -> Scan.Alex b) -> Scan.Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Scan.Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Scan.RangedToken), [Prelude.String]) -> Scan.Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseLox = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Scan.RangedToken -> Scan.Alex a
parseError token = do
  (Scan.AlexPn _ line column, _, _, _) <- Scan.alexGetInput
  Scan.alexError $ "Parse error at line " <> show line <> ", column " <> show column <> ". Found " <> show token

lexer :: (Scan.RangedToken -> Scan.Alex a) -> Scan.Alex a
lexer = (=<< Scan.alexMonadScan)

unTok :: Scan.RangedToken -> (Scan.Range -> Scan.TokenType -> a) -> a
unTok (Scan.RangedToken token range) f = f range token

-- | Does not check if end of first range equals start of second range by design
(<->) :: Scan.Range -> Scan.Range -> Scan.Range
(Scan.Range a _) <-> (Scan.Range _ d) = Scan.Range a d
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
