{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Fortran.Vars.Types
  ( module Language.Fortran.Vars.Types.SymbolTable
  , module Language.Fortran.Vars.Types
  , Type
  , SemType(..)
  , Dim(..), Dims(..), Dimensions, dimensionsToTuples
  , dimsTraverse, dimsLength
  , CharacterLen(..)
  , Kind
  , ExpVal(..)
  )
where

import Language.Fortran.Vars.Types.SymbolTable

import           Language.Fortran.Common.Array ( dimsTraverse, dimsLength )
import           Language.Fortran.Vars.Orphans()
import           Language.Fortran.Vars.Rep
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

instance ToJSON TypeError
instance FromJSON TypeError

-- | Construct a 'TypeError' using a 'SrcSpan', using the 'FilePath'.
typeError :: SrcSpan -> String -> TypeError
typeError sp = let SrcSpan p _ = sp in TypeError (posFilePath p) sp

type TypeOf a = Expression a -> Either TypeError Type

dimensionsToTuples' :: Dimensions -> [(Int, Int)]
dimensionsToTuples' dims =
    case dimensionsToTuples dims of
      Nothing    -> []
      Just dims' -> dims'

-- | Attempt to turn a list of evaluated array bounds which may include unknown
--   bounds, into a list of known bounds. Any unknown bounds will result in a
--   'Nothing'.
getStaticArrayBounds :: Traversable t => Dims t (Maybe a) -> Maybe (Dims t a)
getStaticArrayBounds = dimsTraverse
