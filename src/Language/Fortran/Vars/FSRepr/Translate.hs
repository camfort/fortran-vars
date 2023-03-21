{- TODO
  * BYTE is apparently LOGICAL(1). Or INTEGER(1) (same thing?). Could make a
    special check for that.
-}

module Language.Fortran.Vars.FSRepr.Translate where

import qualified Language.Fortran.Vars.Repr as FV
import qualified Language.Fortran.AST.Literal.Boz as AST

import Language.Fortran.Repr
import Language.Fortran.Repr.Type.Array

import GHC.Float ( float2Double )
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty

translateFType :: FType -> FV.SemType
translateFType = \case
  MkFScalarType fsty -> translateFScalarType fsty
  MkFArrayType  fat  -> translateFArrayType  fat

translateFScalarType :: FScalarType -> FV.SemType
translateFScalarType = \case
  FSTInt     ftint  -> kinded FV.TInteger ftint
  FSTReal    ftreal -> kinded FV.TReal    ftreal
  FSTComplex ftreal -> kinded FV.TComplex (FTComplexWrapper ftreal)
  FSTLogical ftint  -> kinded FV.TInteger ftint
  FSTString  n      -> FV.TCharacter (FV.CharLenInt (fromIntegral n)) 1
  FSTCustom  ty     -> FV.TCustom ty
  where kinded f = f . translateFKind . printFKind

translateFArrayType :: FArrayType -> FV.SemType
translateFArrayType (FArrayType fsty shape) =
    FV.TArray (translateFScalarType fsty) (translateShape shape)

translateFKind :: FKindLit -> FV.Kind
translateFKind = fromIntegral

-- | Note that Fortran defaults to 1-indexed arrays.
translateShape :: Shape -> FV.Dimensions
translateShape =
    FV.DimsExplicitShape . NonEmpty.fromList . map (\ub -> FV.Dim 1 (fromIntegral ub)) . getShape

--------------------------------------------------------------------------------

translateFValue :: FValue -> Either String FV.ExpVal
translateFValue = \case
  MkFScalarValue fsv  -> translateFScalarValue fsv

translateFScalarValue :: FScalarValue -> Either String FV.ExpVal
translateFScalarValue = \case
  FSVInt     fint      -> Right $ FV.Int  $ fIntUOp fromIntegral fint
  FSVReal    freal     -> Right $ FV.Real $ fRealUOp' float2Double id freal
  FSVComplex _fcomplex -> Left "ExpVal doesn't support complex values"
  FSVLogical fint -> Right $ FV.Logical $ fLogicalToBool fint
  FSVString  t -> Right $ FV.Str $ Text.unpack t

--------------------------------------------------------------------------------

translateExpVal :: FV.ExpVal -> FScalarValue
translateExpVal = \case
  FV.Int     i   -> FSVInt     $ FInt4 $ fromIntegral i

  -- TODO getting some precisions errors, fortran-src over-precise? unsure where
  -- coming from, but need to compare using an epsilon
  FV.Real    r   -> FSVReal    $ FReal8 r

  FV.Str     s   -> FSVString  $ Text.pack s
  FV.Logical b   -> FSVLogical $ FInt4 $ fLogicalNumericFromBool b

  -- TODO fortran-vars always converts BOZs at INTEGER(2)
  FV.Boz     boz -> FSVInt     $ FInt2 $ AST.bozAsTwosComp boz
