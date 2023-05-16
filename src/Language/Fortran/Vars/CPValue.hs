{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fortran.Vars.CPValue
  ( CPValue(..)
  , meet
  , unaryOper
  , binaryOper
  , isTop
  , isBot
  , isConstInt
  )
where

import           Language.Fortran.Vars.Types
                                                ( ExpVal(..) )
import           Language.Fortran.Vars.Eval.Deprecated.Operation
                                                ( unaryOp
                                                , binaryOp
                                                )

import           Language.Fortran.AST           ( UnaryOp(..)
                                                , BinaryOp(..)
                                                )

import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )

-- | CPValue (Constant Propagation Value) represnts the value of an expression
-- determined by constant propagation analysis.
-- The value can be uninitialized, a constant, or unknown due to conflict.
-- The data type is represented with 'ExpVal' together with two special values:
-- Top represents uninitialized value and is the least upper bound
-- Bot represents unknown and is the greatest lower bound
-- Top, Const, and Bot forms a lattice strucutre with meet operation defined below
data CPValue
  = Top           -- ^ represents uninitialized value
  | Const ExpVal  -- ^ represents a constant value
  | Bot           -- ^ short for bottom, represents unknown value
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)


-- | meet operation for CPValue lattice with user defined function to handle
-- two Const values
meetWith :: (ExpVal -> ExpVal -> CPValue) -> CPValue -> CPValue -> CPValue
meetWith f v1 v2 = case (v1, v2) of
  (Bot     , _       ) -> Bot
  (_       , Bot     ) -> Bot
  (Top     , v       ) -> v
  (v       , Top     ) -> v
  (Const c1, Const c2) -> f c1 c2

-- | meet operation for CPValue lattice.
-- meet of two different constant value indicates conflict, therefore yields Bot.
meet :: CPValue -> CPValue -> CPValue
meet = meetWith (\c1 c2 -> if c1 == c2 then Const c1 else Bot)


unaryOper :: UnaryOp -> CPValue -> CPValue
unaryOper op v = case v of
  Top          -> Top
  Bot          -> Bot
  Const expVal -> Const (unaryOp op expVal)

binaryOper :: BinaryOp -> CPValue -> CPValue -> CPValue
binaryOper op v1 v2 = case (op, v1, v2) of
  (Multiplication, Const (Int 0), _) -> Const (Int 0)
  (Multiplication, _, Const (Int 0)) -> Const (Int 0)
  (Multiplication, Const (Real 0.0), _) -> Const (Real 0.0)
  (Multiplication, _, Const (Real 0.0)) -> Const (Real 0.0)
  (Exponentiation, Const (Real 1.0), _) -> Const (Real 1.0)
  (Exponentiation, Const (Int 1), _) -> Const (Real 1.0)
  (Exponentiation, _, Const (Real 0.0)) -> Const (Real 1.0)
  (Exponentiation, _, Const (Int 0)) -> Const (Real 1.0)
  (And, Const (Logical False), _) -> Const (Logical False)
  (And, _, Const (Logical False)) -> Const (Logical False)
  (Or, Const (Logical True), _) -> Const (Logical True)
  (Or, _, Const (Logical True)) -> Const (Logical True)
  (_, Bot, _) -> Bot
  (_, _, Bot) -> Bot
  (_, Top, _) -> Top
  (_, _, Top) -> Top
  (_, Const x, Const y) -> Const (binaryOp op x y)


isTop :: CPValue -> Bool
isTop Top = True
isTop _   = False

isBot :: CPValue -> Bool
isBot Bot = True
isBot _   = False

isConstInt :: CPValue -> Bool
isConstInt (Const Int{}) = True
isConstInt _             = False
