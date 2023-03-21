{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Vars.Orphans where

import Language.Fortran.AST
import qualified Language.Fortran.AST.Literal.Boz as Boz
import Language.Fortran.Util.Position
import Language.Fortran.Analysis.SemanticTypes
import Language.Fortran.Extras.JSON.Analysis()

import Data.Aeson ( ToJSON, FromJSON, ToJSONKey, FromJSONKey )

-- TODO temporary solution
instance   ToJSON SemType
instance FromJSON (Dim Int)
instance FromJSON Dimensions
instance FromJSON SemType
instance   ToJSON CharacterLen
instance FromJSON CharacterLen

instance FromJSON SrcSpan
instance FromJSON Position

instance FromJSON Boz.Boz
instance FromJSON Boz.BozPrefix
instance FromJSON Boz.Conforming

-- TODO move these to common
instance ToJSON ProgramUnitName
instance ToJSONKey ProgramUnitName
instance FromJSON ProgramUnitName
instance FromJSONKey ProgramUnitName
