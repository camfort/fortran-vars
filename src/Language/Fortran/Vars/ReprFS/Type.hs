{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- TODO
  * bit more automation using the FKinded etc. defs (largely type level) in FS
  * clean up errors
  * document approach, possible pitfalls
-}

module Language.Fortran.Vars.ReprFS.Type where

import qualified Language.Fortran.Repr as FS

import qualified Language.Fortran.Analysis.SemanticTypes as FV

--import qualified Language.Fortran.Vars.FSRepr.Translate as FS

pattern TInteger :: FV.Kind -> FS.FType
pattern TInteger k <- FS.MkFScalarType (FS.FSTInt (fsIntToFVKind -> k))
  where TInteger k =  FS.MkFScalarType (FS.FSTInt (fvKindToFSInt k))

fvKindToFSInt :: FV.Kind -> FS.FTInt
fvKindToFSInt = \case
  1  -> FS.FTInt1
  2  -> FS.FTInt2
  4  -> FS.FTInt4
  8  -> FS.FTInt8
  16 -> FS.FTInt16
  _  -> error "TODO"

fsIntToFVKind :: FS.FTInt -> FV.Kind
fsIntToFVKind = \case
  FS.FTInt1  -> 1
  FS.FTInt2  -> 2
  FS.FTInt4  -> 4
  FS.FTInt8  -> 8
  FS.FTInt16 -> 16

pattern TReal :: FV.Kind -> FS.FType
pattern TReal k <- FS.MkFScalarType (FS.FSTReal (fsRealToFVKind -> k))
  where TReal k =  FS.MkFScalarType (FS.FSTReal (fvKindToFSReal k))

fvKindToFSReal :: FV.Kind -> FS.FTReal
fvKindToFSReal = \case
  4 -> FS.FTReal4
  8 -> FS.FTReal8
  _ -> error "TODO"

fsRealToFVKind :: FS.FTReal -> FV.Kind
fsRealToFVKind = \case
  FS.FTReal4 -> 4
  FS.FTReal8 -> 8

-- TODO correct? I forget
pattern TComplex :: FV.Kind -> FS.FType
pattern TComplex k <- FS.MkFScalarType (FS.FSTReal (fsRealToFVKind -> k))
  where TComplex k =  FS.MkFScalarType (FS.FSTComplex (fvKindToFSReal k))

pattern TLogical :: FV.Kind -> FS.FType
pattern TLogical k <- FS.MkFScalarType (FS.FSTInt (fsIntToFVKind -> k))
  where TLogical k =  FS.MkFScalarType (FS.FSTInt (fvKindToFSInt k))

-- TODO identical to Int
pattern TByte :: FV.Kind -> FS.FType
pattern TByte k <- FS.MkFScalarType (FS.FSTInt (fsIntToFVKind -> k))
  where TByte k =  FS.MkFScalarType (FS.FSTInt (fvKindToFSInt k))

{- TODO how to handle kind?? want to enforce kind=1, or ignore if not possible
pattern TCharacter :: FV.CharacterLen -> FV.Kind -> FS.FType
pattern TCharacter len k <- FS.MkFScalarType (FS.FSTString (fromIntegral -> FV.CharLenInt len))
  where TCharacter (FV.CharLenInt len) k =  FS.MkFScalarType (FS.FSTString (fromIntegral len))
-}

{- TODO fiddly
pattern TArray :: FV.SemType -> Maybe FV.Dimensions -> FS.FType
pattern TArray ty mdims <- FS.MkFArrayType (FS.FArrayType (FS.translateFType -> FS.MkFScalarType ty) mdims)
-}
