{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Vars.Types
  ( module Language.Fortran.Vars.Types
  , SemType(..)
  , CharacterLen(..)
  , Kind
  )
where

import           Language.Fortran.Extras.Encoding
                                                ( )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , ToJSONKey
                                                , FromJSONKey
                                                )
import           Data.Data                      ( Data )
import           Data.Map                       ( Map )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import           Language.Fortran.AST           ( Name
                                                , ProgramUnitName
                                                , Expression
                                                , Kind
                                                )
import qualified Language.Fortran.AST.Boz      as AST
import           Language.Fortran.Util.Position ( SrcSpan(..)
                                                , Position(..)
                                                )
import           Language.Fortran.Analysis.SemanticTypes
                                                ( SemType(..)
                                                , CharacterLen(..)
                                                )

type Type = SemType

-- | The evaluated value of a FORTRAN expression
data ExpVal
  = Int     Int
  | Real    Double
  | Str     String
  | Logical Bool
  | Boz     AST.Boz
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

-- instance FromJSON AST.Conforming
-- instance ToJSON AST.Conforming
instance FromJSON AST.Boz
instance ToJSON AST.Boz
instance FromJSON AST.BozPrefix
instance ToJSON AST.BozPrefix
instance FromJSON ExpVal
instance ToJSON ExpVal

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

-- | The declared dimensions of a staticically typed array variable
-- type is of the form [(dim1_lower, dim1_upper), (dim2_lower, dim2_upper)]
type Dimensions = [(Int, Int)]

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

-- move these to common
instance ToJSON ProgramUnitName
instance ToJSONKey ProgramUnitName
instance FromJSON ProgramUnitName
instance FromJSONKey ProgramUnitName

data TypeError
  = TypeError FilePath SrcSpan String
  | UnknownType SrcSpan
  | UnboundVariable Name
  | UnknownField String
  deriving (Eq, Ord, Show, Generic)

-- | Helper method for getting the FilePath out of SrcSpan
typeError :: SrcSpan -> String -> TypeError
typeError sp = let SrcSpan p _ = sp in TypeError (filePath p) sp

instance ToJSON TypeError
instance FromJSON TypeError

type TypeOf a = Expression a -> Either TypeError Type
