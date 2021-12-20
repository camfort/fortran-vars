{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.Vars.StructureTable
  ( StructureTable
  , StructureTableEntry(..)
  , collectStructures
  , lookupField
  , hasEntry
  , programStructureTables
  )
where

import           Data.Data                      ( Data )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Map                      as M
import           Data.List                      ( foldl' )

import           Language.Fortran.Analysis      ( Analysis
                                                , puName
                                                )
import           Language.Fortran.AST           ( Statement(..)
                                                , StructureItem(..)
                                                , UnionMap(..)
                                                , Expression(..)
                                                , Value(..)
                                                , ProgramUnit(..)
                                                , ProgramFile
                                                , TypeSpec(..)
                                                , Declarator(..)
                                                , DeclaratorType(..)
                                                , aStrip
                                                )
import           Language.Fortran.Extras
                                                ( allPUS
                                                , allPU
                                                )

import           Language.Fortran.Vars.SymbolTable
                                                ( collectSymbols )
import           Language.Fortran.Vars.Types
                                                ( SymbolTable
                                                , StructureTableEntry(..)
                                                , Structure
                                                , StructureTable
                                                , ProgramStructureTables
                                                , Type(..)
                                                , SemType(..)
                                                , TypeError(..)
                                                )
import           Language.Fortran.Vars.Utils
                                                ( typeSpecToArrayType
                                                , typeSpecToScalarType
                                                )

-- | Given a `SymbolTable` transform `StructureItem`s found in the AST into a list of
-- `StructureTableEntry`s
itemToEntry
  :: SymbolTable -> StructureItem (Analysis a) -> [StructureTableEntry]
itemToEntry st (StructFields _ _ ty _ decls) =
  mapMaybe (handleDeclarator st ty) (aStrip decls)
itemToEntry st (StructUnion _ _ l) = [UnionEntry $ handleUnion st <$> aStrip l]
itemToEntry _  StructStructure{}   = []

-- TODO take into account length, should override default typespecs
-- | Given the 'TypeSpec' and 'Declarator' found in a field entry create a
--   'StructureTableEntry'
handleDeclarator
  :: SymbolTable
  -> TypeSpec (Analysis a)
  -> Declarator (Analysis a)
  -> Maybe StructureTableEntry
handleDeclarator st ty (Declarator _ _ expr ScalarDecl _ _) =
  let scalarTy = typeSpecToScalarType st ty
  in  expToName expr >>= \name -> Just $ FieldEntry name scalarTy
handleDeclarator st ty (Declarator _ _ expr (ArrayDecl dims) _ _) =
  let arrayty = typeSpecToArrayType st (aStrip dims) ty
  in  expToName expr >>= \name -> Just $ FieldEntry name arrayty

-- | Transform a `UnionMap` in an AST to `StructureTableEntry`s
handleUnion :: SymbolTable -> UnionMap (Analysis a) -> [StructureTableEntry]
handleUnion st (UnionMap _ _ si) = concatMap (itemToEntry st) $ aStrip si

-- | Given an `Expression` maybe get the name
expToName :: Expression a -> Maybe String
expToName (ExpValue _ _ (ValVariable name)) = Just name
expToName _ = Nothing

-- | Collect structures defined in a `ProgramUnit` and return a `StructureTable`
collectStructures
  :: Data a => SymbolTable -> ProgramUnit (Analysis a) -> StructureTable
collectStructures st pu =
  M.foldlWithKey' handler M.empty
    . M.fromList
    $ [ (n, aStrip s) | (StStructure _ _ (Just n) s) <- allPUS pu ]
 where
  handler structTable name entry =
    M.insert name (concatMap (itemToEntry st) entry) structTable

-- | Look up reference on a type to return another `Type`
lookupField :: StructureTable -> Type -> String -> Either TypeError Type
lookupField structTable ty ref = case ty of
  TCustom tyName -> case M.lookup tyName structTable of
    Just struct -> hasEntry ref struct
    Nothing     -> Left . UnknownField $ ref <> " not a field of " <> tyName
  ty' -> Left . UnknownField $ "No fields for data type (" <> show ty' <> ")"

-- | Given a name, check that a `Structure` contains it once and return its
-- corresponding `Type`
hasEntry :: String -> Structure -> Either TypeError Type
hasEntry name struct =
  let unionStructs = concat [ concat structs | (UnionEntry structs) <- struct ]
  in
    case
      [ ty | (FieldEntry fname ty) <- struct <> unionStructs, fname == name ]
    of
      [ty] -> Right ty
      []   -> Left . UnknownField $ name <> " is not a field"
      _ ->
        Left
          .  UnknownField
          $  "Field "
          <> name
          <> " define multiple times for structure"

-- | Given a 'ProgramFile', generate a 'StructureTable' for each 'ProgramUnit'.
-- This can be used to check types in data reference expressions
programStructureTables
  :: Data a => ProgramFile (Analysis a) -> ProgramStructureTables
programStructureTables pf = foldl' handler M.empty $ allPU pf
 where
  handler m pu = M.insert (puName pu) (puStructure pu) m
  puStructure pu = let st = collectSymbols pu in collectStructures st pu
