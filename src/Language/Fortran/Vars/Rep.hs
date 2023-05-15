-- | Definitions for representing Fortran values and types.

module Language.Fortran.Vars.Rep
  (
  -- * Types
    F.SemType(..)
  , F.Kind
  , F.CharacterLen(..)
  , F.Dimensions, F.Dim(..), F.Dims(..)

  -- ** Compatibility
  , F.dimensionsToTuples
  , Type

  -- * Values
  , ExpVal(..)
  ) where

import qualified Language.Fortran.Analysis.SemanticTypes as F
import qualified Language.Fortran.AST.Literal.Boz as F
import Language.Fortran.Extras.JSON.Literals()

import Language.Fortran.Vars.Orphans()

import Data.Aeson ( ToJSON, FromJSON )
import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data )

-- TODO raehik 2023-05-15: consider deprecating. GHC is very gradually changing
-- the kind of concrete types from @*@ to @Type@. Language extension
-- @NoStarIsType@ makes that change, and will eventually become default. @type
-- Type@ will probably break with that on (due to the way kind/type/term
-- namespaces are searched).
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
