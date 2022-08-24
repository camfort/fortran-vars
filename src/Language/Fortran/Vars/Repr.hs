module Language.Fortran.Vars.Repr
  ( SemType
  , Type
  , module Language.Fortran.Vars.ReprFS.Type
  , FVOld.CharacterLen(..)
  , ExpVal
  , module Language.Fortran.Vars.ReprFS.Value
  , FVOld.Kind
  , FVOld.Dimensions
  ) where

import qualified Language.Fortran.Repr as FS
import Language.Fortran.Vars.ReprFS.Type
import Language.Fortran.Vars.ReprFS.Value
import qualified Language.Fortran.Analysis.SemanticTypes as FVOld

type SemType = FS.FType
type Type    = SemType
type ExpVal = FS.FScalarValue
