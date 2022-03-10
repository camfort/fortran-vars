module Language.Fortran.Vars.BozConstant
  ( resolveBozConstant
  , bozToInt
  , bozToInt2
  , bozToInt4
  , bozToInt8
  )
where

import qualified Data.Map                      as M

import qualified Language.Fortran.AST.Boz      as AST
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , SemType(..)
                                                , Kind
                                                , ExpVal(..)
                                                , SymbolTable
                                                )

-- | Given 'SymbolTable', contextual symbol name and BOZ Constant
-- ('ExpVal' constructed with Boz String), resolve BOZ Constant considering
-- contextual symbol.
--
-- Currently, it only resolves BOZ Constants in context of INTEGER.
resolveBozConstant :: SymbolTable -> String -> ExpVal -> ExpVal
resolveBozConstant symTable assignSymbol (Boz boz) =
  resolveBozConstant' symTable assignSymbol boz
resolveBozConstant _ _ _ = error "Can only resolve ExpVal Boz"

resolveBozConstant' :: SymbolTable -> String -> AST.Boz -> ExpVal
resolveBozConstant' symTable assignSymbol boz =
  let entry = M.lookup assignSymbol symTable
  in
    case entry of
      Just (SVariable (TInteger kind) _) ->
        resolveBozConstantInContext boz kind
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

resolveBozConstantInContext :: AST.Boz -> Kind -> ExpVal
resolveBozConstantInContext boz kind =
  let allowedBinaryLength = kind * 8
      maxBinaryValue      = 2 ^ (allowedBinaryLength - 1) - 1
      minBinaryValue      = (-1) * 2 ^ (allowedBinaryLength - 1)
      decimal             = AST.bozAsNatural boz
      overflow            = decimal - maxBinaryValue
  in  if overflow > 0 then Int (minBinaryValue + overflow - 1) else Int decimal

-- Convert BOZ string to integer of specific kind
bozToInt :: Int -> AST.Boz -> ExpVal
bozToInt kind boz = resolveBozConstantInContext boz kind

-- Convert BOZ string to integer*2
bozToInt2 :: AST.Boz -> ExpVal
bozToInt2 = bozToInt 2

-- Convert BOZ string to integer*4
bozToInt4 :: AST.Boz -> ExpVal
bozToInt4 = bozToInt 4

-- Convert BOZ string to integer*8
bozToInt8 :: AST.Boz -> ExpVal
bozToInt8 = bozToInt 8
