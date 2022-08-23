{- TODO
  * BYTE is apparently LOGICAL(1). Or INTEGER(1) (same thing?). Could make a
    special check for that.
-}

module Language.Fortran.Vars.FSRepr.Translate where

import Language.Fortran.Repr
import Language.Fortran.Repr.Eval.Op
--import qualified Language.Fortran.Analysis.SemanticTypes as FV
import qualified Language.Fortran.Vars.Repr as FV
import GHC.Float ( float2Double )
import qualified Data.Text as Text

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
    FV.TArray (translateFScalarType fsty) (Just (translateShape shape))

translateFKind :: FKindTerm -> FV.Kind
translateFKind = fromIntegral

-- | Note that Fortran defaults to 1-indexed arrays.
translateShape :: Shape -> FV.Dimensions
translateShape = map (\n -> (defaultArrayStartIndex, translateFKind n)) . getShape
  where
    -- TODO add to fortran-src somewhere
    defaultArrayStartIndex = 1

--------------------------------------------------------------------------------

translateFValue :: FValue -> Either String FV.ExpVal
translateFValue = \case
  MkFScalarValue fsv  -> translateFScalarValue fsv
  MkFArrayValue  _fav -> Left "ExpVal doesn't support array values"

translateFScalarValue :: FScalarValue -> Either String FV.ExpVal
translateFScalarValue = \case
  FSVInt     fint      -> Right $ FV.Int  $ someFIntUOp  fromIntegral fint
  FSVReal    freal     -> Right $ FV.Real $ someFRealUOp' float2Double id freal
  FSVComplex _fcomplex -> Left "ExpVal doesn't support complex values"
  FSVLogical (SomeFKinded fint) -> Right $ FV.Logical $ fLogicalToBool fint
  FSVString  (SomeFString (FString t)) -> Right $ FV.Str $ Text.unpack t
