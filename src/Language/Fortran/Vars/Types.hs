{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Fortran.Vars.Types
  ( module Language.Fortran.Vars.Types
  , Type
  , SemType(..)
  , Dim(..), Dims(..), Dimensions, dimensionsToTuples, dimensionsToTuples'
  , CharacterLen(..)
  , Kind
  , ExpVal(..)
  )
where

import           Language.Fortran.Vars.Orphans()
import           Language.Fortran.Vars.Repr
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Data                      ( Data )
import           Data.Map                       ( Map )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( Name
                                                , ProgramUnitName
                                                , Expression
                                                )
import           Language.Fortran.Util.Position ( SrcSpan(..)
                                                , Position(..)
                                                )

-- | Memory offset given to a variable in memory
type Offset = Int

-- | The name of block of memory
type MemoryBlockName = Name

-- | The location of a variable, i.e. the 'MemoryBlockName' that
-- contains it as well as the 'Offset' to its location in memory
type Location = (MemoryBlockName, Offset)

-- | The declared lifetimes of the variables in memory
data StorageClass
  = Static
  | Automatic
  | Constant
  | Common
  | Unspecified
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON StorageClass
instance ToJSON StorageClass

-- | An entry in the 'SymbolTable' for some variable
data SymbolTableEntry
  = SParameter { parType :: Type , parVal :: ExpVal }
  | SVariable { varType :: Type , varLoc :: Location }
  | SDummy { dumType :: Type }
  | SExternal {extType :: Type }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON SymbolTableEntry
instance ToJSON SymbolTableEntry

-- | Symbol table containing all non-intrisic symbols declared in a program
type SymbolTable = Map Name SymbolTableEntry

-- | Structure to hold information about the named blocks of memory
-- in the program
data MemoryBlock = MemoryBlock
  { blockSize    :: Maybe Int -- ^ Nothing for when block is dynamically sized
  , storageClass :: StorageClass
  , variables    :: [Name]
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON MemoryBlock
instance ToJSON MemoryBlock

-- | Map from a structure name to its internal structure, specifying members
-- and their corresponding type. This can then be used to check the type of a
-- data reference expression.
type StructureTable = Map String Structure

-- List of structure fields forming a structure
type Structure = [StructureTableEntry]

-- | Data structurue for a single field of a structure
data StructureTableEntry
  = FieldEntry String Type
  | UnionEntry [Structure]
  deriving (Eq, Show, Data)

-- | Mapping from the name of a memory block to the information about it
type StorageTable = Map MemoryBlockName MemoryBlock

-- | The model to represent an individual 'Language.Fortran.AST.ProgramUnit'
type ProgramUnitModel = (SymbolTable, StorageTable)

-- | Mapping from the name of a 'Language.Fortran.AST.ProgramUnit' to
-- its 'ProgramUnitModel'
type ProgramFileModel = Map ProgramUnitName ProgramUnitModel

-- | Mapping from name of a program unit to relevant structure table
type ProgramStructureTables = Map ProgramUnitName StructureTable

data TypeError
  = TypeError FilePath SrcSpan String
  | UnknownType SrcSpan
  | UnboundVariable Name
  | UnknownField String
  deriving (Eq, Ord, Show, Generic)

-- | Helper method for getting the FilePath out of SrcSpan
typeError :: SrcSpan -> String -> TypeError
typeError sp = let SrcSpan p _ = sp in TypeError (posFilePath p) sp

instance ToJSON TypeError
instance FromJSON TypeError

type TypeOf a = Expression a -> Either TypeError Type

dimensionsToTuples' :: Dimensions -> [(Int, Int)]
dimensionsToTuples' dims =
    case dimensionsToTuples dims of
      Nothing    -> []
      Just dims' -> dims'
