module Language.Fortran.Vars.Operation
  ( valueToExpVal'
  , valueToExpVal
  , transformEither
  , transformEitherList
  , binaryTransformEither
  , unaryOp'
  , unaryOp
  , binaryOp'
  , binaryOp
  , intrinsicFunctionCall
  , nonLogicalToLogical
  )
where

import           Prelude                 hiding ( GT
                                                , EQ
                                                , LT
                                                )
import           Data.Char                      ( toUpper
                                                , chr
                                                )
import           Data.Either                    ( either )
import           Text.Read                      ( readMaybe )

import           Language.Fortran.AST           ( BinaryOp(..)
                                                , UnaryOp(..)
                                                , Value(..)
                                                )
import           Language.Fortran.AST.RealLit   ( readRealLit )
import           Language.Fortran.AST.Boz       ( prettyBoz )
import           Language.Fortran.Util.Position ( SrcSpan )


import           Language.Fortran.Vars.BozConstant
                                                ( bozToInt8
                                                , bozToInt
                                                )
import           Language.Fortran.Vars.Errors
                                                ( invalidArg' )
import           Language.Fortran.Vars.Types
                                                ( ExpVal(..) )

import           Data.Bits                      ( (.|.)
                                                , complement
                                                )

-- | Given a function that returns an 'Either' and an 'Either' with
-- the 'Right' case as the same type input to the function, return
-- an either by possibly applying the function to the 'Right' value or
-- propagating the 'Left' case
transformEither :: (a -> Either String b) -> Either String a -> Either String b
transformEither = either Left
{-# INLINABLE transformEither #-}

-- | Given a function that takes two arguments of the same type and returns an
-- 'Either' as well as two 'Either's whose 'Right' cases hold the inputs to the
-- function, apply the function if possible. Otherwise propagate the 'Left' cases
binaryTransformEither
  :: (a -> a -> Either String b)
  -> Either String a
  -> Either String a
  -> Either String b
binaryTransformEither _ (Left e)   _          = Left e
binaryTransformEither _ _          (Left  e ) = Left e
binaryTransformEither t (Right v1) (Right v2) = t v1 v2

-- | Given a function that takes a list of arguments of the same type and returns an
-- 'Either' as well as a list of 'Either's whose 'Right' cases hold the inputs to the
-- function, apply the function if possible. Otherwise propagate the 'Left' cases
transformEitherList
  :: ([a] -> Either String b) -> [Either String a] -> Either String b
transformEitherList t el = case eitherListToList el of
  Left  l  -> Left l
  Right rs -> t rs
 where
  eitherListToList :: [Either String a] -> Either String [a]
  eitherListToList []             = Right []
  eitherListToList (Left  l : _ ) = Left l
  eitherListToList (Right r : rs) = case eitherListToList rs of
    Left  l   -> Left l
    Right rs' -> Right (r : rs')

-- | Given a 'SrcSpan' and the 'Value' in that span either
-- return a 'String' describing the issue or the 'ExpVal' held
-- by that 'Value'.
valueToExpVal' :: SrcSpan -> Value a -> Either String ExpVal
valueToExpVal' s val = case val of
  ValInteger   i  _ -> Right $ Int     $ read i
  ValReal      r  _ -> Right $ Real    $ readRealLit r
  ValLogical   l  _ -> Right $ Logical l
  ValString    s'   -> Right $ Str s'
  ValHollerith h    -> Right $ Str h
  ValBoz       b    -> Right $ Boz b
  _               -> Left ("toExpVal: unsupported value at " ++ show s)

-- | Given a 'SrcSpan' and the 'Value' returnthe 'ExpVal' held
-- by that 'Value' or throw an error.
valueToExpVal :: SrcSpan -> Value a -> ExpVal
valueToExpVal s val = case valueToExpVal' s val of
  Left  err  -> error err
  Right expr -> expr

-- | Given a non-logical 'ExpVal', convert that value to a logical
-- one or return a 'String' describing why this was impossible.
nonLogicalToLogical :: ExpVal -> Either String Bool
nonLogicalToLogical (Int  i) = Right $ i /= 0
nonLogicalToLogical (Real r) = Right $ r /= 0.0
nonLogicalToLogical (Str _) =
  Left "Cannot transform a string value to a logical value"
nonLogicalToLogical (  Logical l) = Right l
nonLogicalToLogical b@(Boz     _) = nonLogicalToLogical $ bozToInt8 b

-- | Given a string representing a function call and a list of ExpVal
-- values holding inputs to the function, evaluate the function call
-- and return the result in a Right, or propagate the Left case if any
-- of the list elements are 'Lefts'.
intrinsicFunctionCall :: String -> [ExpVal] -> Either String ExpVal
intrinsicFunctionCall function es = case function of
  "ior"  -> ior' es
  "max"  -> max' es
  "char" -> char' es
  "not"  -> not' es
  "int"  -> int' es
  "int2" -> int' es
  _      -> invalidArg' ("intrinsicFunctionCall " ++ show function) es

ior' :: [ExpVal] -> Either String ExpVal
ior' [val1, val2] = case (val1, val2) of
  (Int a, Int b) -> Right $ Int $ (.|.) a b
  _              -> invalidArg' "ior" [val1, val2]
ior' vs = invalidArg' "ior" vs

max' :: [ExpVal] -> Either String ExpVal
max' [val1] = case val1 of
  Real a -> Right $ Real a
  Int  a -> Right $ Int a
  _      -> invalidArg' "max" [val1]
max' (v : vs) =
  let maxVs = max' vs
  in  case (v, maxVs) of
        (_      , Left l        ) -> Left l
        (Real r', Right (Int r) ) -> Right $ Real $ max r' (fromIntegral r)
        (Int  r', Right (Real r)) -> Right $ Real $ max (fromIntegral r') r
        (Real r', Right (Real r)) -> Right $ Real $ max r' r
        (Int  r', Right (Int r) ) -> Right $ Int $ max r' r
        _                         -> invalidArg' "max" (v : vs)
max' vs = invalidArg' "max" vs

char' :: [ExpVal] -> Either String ExpVal
char' [Int i] = Right $ Str [chr i]
char' vs      = invalidArg' "char" vs

-- https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnc8/index.html
not' :: [ExpVal] -> Either String ExpVal
not' [Int i] = Right $ Int (complement i)
not' vs      = invalidArg' "not" vs

int' :: [ExpVal] -> Either String ExpVal
int' [Int  i] = Right $ Int i
int' [Real r] = Right $ Int (truncate r)
int' v@[boz@(Boz _), Int k] =
  if k `elem` [2, 4, 8] then Right $ bozToInt k boz else invalidArg' "int" v
int' vs = invalidArg' "int" vs

-- | Given a 'UnaryOp' and an 'ExpVal', either return the resulting
-- 'ExpVal' after applying the operation or a 'String' describing
-- why this couldn't be done
unaryOp' :: UnaryOp -> ExpVal -> Either String ExpVal
unaryOp' op v = case (op, v) of
  (Plus , Int a ) -> Right $ Int a
  (Plus , Real a) -> Right $ Real a
  (Minus, Int a ) -> Right $ Int (negate a)
  (Minus, Real a) -> Right $ Real (negate a)
  (Not, a) -> transformEither (Right . Logical . not) $ nonLogicalToLogical a
  _               -> invalidArg' (show op) [v]

-- | Given a 'UnaryOp' and an 'ExpVal', either return the resulting
-- 'ExpVal' after applying the operation or throw an error
unaryOp :: UnaryOp -> ExpVal -> ExpVal
unaryOp op v = case unaryOp' op v of
  Left  err  -> error err
  Right expr -> expr

-- | Given a 'BinaryOp' and two 'ExpVal's, either return the resulting
-- 'ExpVal' after applying the operation or a 'String' describing
-- why this couldn't be done
binaryOp' :: BinaryOp -> ExpVal -> ExpVal -> Either String ExpVal
binaryOp' op val1 val2 = case (op, val1, val2) of
  (Addition, Int a, Int b) -> Right $ Int (a + b)
  (Addition, Real a, Real b) -> Right $ Real (a + b)
  (Addition, Int a, Real b) -> Right $ Real (fromIntegral a + b)
  (Addition, Real a, Int b) -> Right $ Real (a + fromIntegral b)

  (Subtraction, Int a, Int b) -> Right $ Int (a - b)
  (Subtraction, Real a, Real b) -> Right $ Real (a - b)
  (Subtraction, Int a, Real b) -> Right $ Real (fromIntegral a - b)
  (Subtraction, Real a, Int b) -> Right $ Real (a - fromIntegral b)

  (Multiplication, Int a, Int b) -> Right $ Int (a * b)
  (Multiplication, Real a, Real b) -> Right $ Real (a * b)
  (Multiplication, Int a, Real b) -> Right $ Real (fromIntegral a * b)
  (Multiplication, Real a, Int b) -> Right $ Real (a * fromIntegral b)

  (Division, Int a, Int b) -> Right $ Int (div a b)
  (Division, Real a, Real b) -> Right $ Real (a / b)
  (Division, Int a, Real b) -> Right $ Real (fromIntegral a / b)
  (Division, Real a, Int b) -> Right $ Real (a / fromIntegral b)

  (Exponentiation, Int a, Int b) -> Right $ Int (a ^ b)
  (Exponentiation, Real a, Real b) -> Right $ Real (a ** b)
  (Exponentiation, Int a, Real b) -> Right $ Real (fromIntegral a ** b)
  (Exponentiation, Real a, Int b) -> Right $ Real (a ** fromIntegral b)

  (Concatenation, Str a, Str b) -> Right $ Str (a ++ b)

  (LT, Int a, Int b) -> Right $ Logical (a < b)
  (LT, Real a, Real b) -> Right $ Logical (a < b)
  (LT, Int a, Real b) -> Right $ Logical (fromIntegral a < b)
  (LT, Real a, Int b) -> Right $ Logical (a < fromIntegral b)
  (LT, a@(Boz _), b) -> binaryOp' LT (bozToInt8 a) b
  (LT, a, b@(Boz _)) -> binaryOp' LT a (bozToInt8 b)

  (EQ, Int a, Real b) -> Right $ Logical (fromIntegral a == b)
  (EQ, Real a, Int b) -> Right $ Logical (a == fromIntegral b)
  (EQ, a@(Boz _), b) -> binaryOp' EQ (bozToInt8 a) b
  (EQ, a, b@(Boz _)) -> binaryOp' EQ a (bozToInt8 b)
  (EQ, Logical True, Int b) -> Right $ Logical (1 == b)
  (EQ, Logical False, Int b) -> Right $ Logical (0 == b)
  (EQ, Int a, Logical True) -> Right $ Logical (a == 1)
  (EQ, Int a, Logical False) -> Right $ Logical (a == 0)
  (EQ, Logical True, Real b) -> Right $ Logical (1.0 == b)
  (EQ, Logical False, Real b) -> Right $ Logical (0.0 == b)
  (EQ, Real a, Logical True) -> Right $ Logical (a == 1.0)
  (EQ, Real a, Logical False) -> Right $ Logical (a == 0.0)
  (EQ, v1, v2) -> Right $ Logical (v1 == v2)


  (GT, v1, v2) -> binaryOp' LT v2 v1
  (GTE, v1, v2) -> transformEither (unaryOp' Not) $ binaryOp' LT v2 v1
  (LTE, v1, v2) -> transformEither (unaryOp' Not) $ binaryOp' GT v2 v1

  (NE, v1, v2) -> transformEither (unaryOp' Not) $ binaryOp' EQ v1 v2

  (And, v1, v2) ->
    binaryTransformEither (\x -> Right . Logical . (x &&))
                          (nonLogicalToLogical v1)
      $ nonLogicalToLogical v2

  (Or, v1, v2) ->
    binaryTransformEither (\x -> Right . Logical . (x ||))
                          (nonLogicalToLogical v1)
      $ nonLogicalToLogical v2

  (XOr, Logical a, Logical b) -> Right $ Logical (a /= b)
  (Equivalent, Logical a, Logical b) -> Right $ Logical (a == b)
  (NotEquivalent, Logical a, Logical b) -> Right $ Logical (a /= b)
  _ -> invalidArg' (show op) [val1, val2]

-- | Given a 'BinaryOp' and two 'ExpVal's, either return the resulting
-- 'ExpVal' after applying the operation or throw an error
binaryOp :: BinaryOp -> ExpVal -> ExpVal -> ExpVal
binaryOp op val1 val2 = case binaryOp' op val1 val2 of
  Left  err  -> error err
  Right expr -> expr
