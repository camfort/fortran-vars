module Language.Fortran.Vars.Repr
  ( Type
  , F.SemType(..)
  , F.CharacterLen(..)
  , ExpVal(..)
  , F.Kind
  , F.Dim(..), F.Dims(..)
  , F.Dimensions, F.dimensionsToTuples
  ) where

import qualified Language.Fortran.Analysis.SemanticTypes as F
import qualified Language.Fortran.AST.Literal.Boz as F
import Language.Fortran.Extras.JSON.Literals()

import Language.Fortran.Vars.Orphans()

import Data.Aeson ( ToJSON, FromJSON )
import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data )

-- | WARNING: This type synonym may cause hassle with later GHC versions
--   (>=8.x?), due to the kind of concrete types changing from @*@ to @Type@.
type Type = F.SemType

-- | The evaluated value of a FORTRAN expression.
data ExpVal
  = Int     Int
  | Real    Double
  | Str     String
  | Logical Bool
  | Boz     F.Boz
    deriving stock (Eq, Ord, Show, Data, Generic)
    deriving anyclass (NFData, ToJSON, FromJSON)
