-- | Only the 'SymbolTable' definitions.

module Language.Fortran.Vars.Types.SymbolTable where

import Language.Fortran.Vars.Rep ( Type, ExpVal )

import Language.Fortran.AST ( Name )

import Data.Map ( Map )

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Data ( Data )

-- | Symbol table containing all non-intrisic symbols declared in a program
type SymbolTable = Map Name SymbolTableEntry

-- | An entry in the 'SymbolTable' for some variable
data SymbolTableEntry
  = SParameter { parType :: Type , parVal :: ExpVal }
  | SVariable  { varType :: Type , varLoc :: Location }
  | SDummy     { dumType :: Type }
  | SExternal  { extType :: Type }
  deriving (Eq, Ord, Show, Data, Generic)

instance FromJSON SymbolTableEntry
instance ToJSON SymbolTableEntry

-- | The location of a variable, i.e. the 'MemoryBlockName' that
-- contains it as well as the 'Offset' to its location in memory
type Location = (MemoryBlockName, Offset)

-- | The name of block of memory
type MemoryBlockName = Name

-- | Memory offset given to a variable in memory
type Offset = Int
