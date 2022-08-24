{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Fortran.Vars.ReprFS.Value where

import qualified Language.Fortran.Repr as FS

import GHC.Float ( float2Double )
import qualified Data.Text as Text

pattern Int :: Int -> FS.FScalarValue
pattern Int i <- FS.FSVInt (FS.SomeFKinded (FS.fIntUOp fromIntegral -> i))
  where Int i =  FS.FSVInt (FS.SomeFKinded (FS.FInt4 (fromIntegral i)))

pattern Real :: Double -> FS.FScalarValue
pattern Real r <- FS.FSVReal (FS.SomeFKinded (FS.fRealUOp' float2Double id -> r))
  where Real r =  FS.FSVReal (FS.SomeFKinded (FS.FReal8 r))

pattern Str :: String -> FS.FScalarValue
pattern Str s <- FS.FSVString (FS.SomeFString (FS.FString (Text.unpack -> s)))
  where Str s =  FS.FSVString (FS.someFString (Text.pack s))

pattern Logical :: Bool -> FS.FScalarValue
pattern Logical b <- FS.FSVLogical (FS.SomeFKinded (FS.fLogicalToBool -> b))
  where Logical b =  FS.FSVLogical (FS.SomeFKinded (FS.FInt4 (FS.fLogicalNumericFromBool b)))

{-
pattern Boz :: F.Boz -> FS.FScalarValue
-}
