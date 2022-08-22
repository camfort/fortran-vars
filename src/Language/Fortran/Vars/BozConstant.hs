module Language.Fortran.Vars.BozConstant
  ( resolveBozConstant
  , bozToInt
  , bozToInt1
  , bozToInt2
  , bozToInt4
  , bozToInt8
  )
where

import qualified Data.Map                      as M

import qualified Language.Fortran.AST.Literal.Boz as AST
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , SemType(..)
                                                , Kind
                                                , ExpVal(..)
                                                , SymbolTable
                                                )

import Data.Int

-- | Given 'SymbolTable', contextual symbol name and BOZ Constant
-- ('ExpVal' constructed with Boz String), resolve BOZ Constant considering
-- contextual symbol.
--
-- Currently, it only resolves BOZ Constants in context of INTEGER.
resolveBozConstant :: SymbolTable -> String -> ExpVal -> ExpVal
resolveBozConstant symTable assignSymbol (Boz boz) = go boz
  where
    go boz = case entry of
      Just (SVariable (TInteger kind) _) ->
        bozToInt kind boz
      Just (SVariable ty _) ->
        error
          $  assignSymbol
          <> " is a "
          <> show ty
          <> "\nBOZ constants can only be resolved in an INTEGER context"
      Just _ -> error
        (assignSymbol
        ++ " is not a scalar variable. \
                                          \Invalid fortran sytax"
        )
      Nothing -> error
        (assignSymbol
        ++ " could not be found. \
                                          \Invalid fortran syntax"
        )
    entry = M.lookup assignSymbol symTable

resolveBozConstant _ _ _ = error "Can only resolve ExpVal Boz"

-- | Resolve a BOZ constant as an INTEGER(k).
--
-- Works on arbitrary kinds, including non-standard, assuming that kind
-- indicates size in bytes.
bozToInt :: Kind -> AST.Boz -> ExpVal
bozToInt kind boz = case kind of
  -- handle regular kinds via bitwise operations on sized machine integers,
  -- relying on overflow behaviour
  1 -> bozToInt1 boz
  2 -> bozToInt2 boz
  4 -> bozToInt4 boz
  8 -> bozToInt8 boz

  -- handle irregular kinds via explicit numeric operations
  -- (shouldn't really ever trigger, but no harm)
  k -> bozAsTwosCompExplicit boz k

--------------------------------------------------------------------------------

-- | Resolve a BOZ constant as an INTEGER(1).
bozToInt1 :: AST.Boz -> ExpVal
bozToInt1 = Int . fromIntegral . AST.bozAsTwosComp @Int8

-- | Resolve a BOZ constant as an INTEGER(2).
bozToInt2 :: AST.Boz -> ExpVal
bozToInt2 = Int . fromIntegral . AST.bozAsTwosComp @Int16

-- | Resolve a BOZ constant as an INTEGER(4).
bozToInt4 :: AST.Boz -> ExpVal
bozToInt4 = Int . fromIntegral . AST.bozAsTwosComp @Int32

-- | Resolve a BOZ constant as an INTEGER(8).
bozToInt8 :: AST.Boz -> ExpVal
bozToInt8 = Int . fromIntegral . AST.bozAsTwosComp @Int64

--------------------------------------------------------------------------------

bozAsTwosCompExplicit :: AST.Boz -> Kind -> ExpVal
bozAsTwosCompExplicit boz kind =
  let allowedBinaryLength = kind * 8
      maxBinaryValue      = 2 ^ (allowedBinaryLength - 1) - 1
      minBinaryValue      = (-1) * 2 ^ (allowedBinaryLength - 1)
      decimal             = AST.bozAsNatural boz
      overflow            = decimal - maxBinaryValue
  in  if overflow > 0 then Int (minBinaryValue + overflow - 1) else Int decimal
