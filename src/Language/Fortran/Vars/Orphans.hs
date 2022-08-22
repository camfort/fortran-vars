module Language.Fortran.Vars.Orphans where

import Language.Fortran.AST
import qualified Language.Fortran.AST.Literal.Boz as Boz
import Language.Fortran.Util.Position
import Language.Fortran.Analysis.SemanticTypes

import Data.Aeson ( ToJSON, FromJSON )

-- TODO temporary solution
instance   ToJSON SemType
instance FromJSON SemType
instance   ToJSON CharacterLen
instance FromJSON CharacterLen

instance FromJSON SrcSpan
instance FromJSON Position

instance FromJSON Boz.Boz
instance FromJSON Boz.BozPrefix
instance FromJSON Boz.Conforming
