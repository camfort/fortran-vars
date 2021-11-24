{-# LANGUAGE TupleSections #-}

module Language.Fortran.Vars.Memory
  ( allocateMemoryBlocks
  , processCommon
  )
where

import           Language.Fortran.Extras
                                                ( allPUS )
import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList(..)
                                                , aStrip
                                                , CommonGroup(..)
                                                , Expression(..)
                                                , Name
                                                , ProgramUnit
                                                , Statement(..)
                                                , Declarator(..)
                                                )

import           Language.Fortran.Vars.MemoryLocation
                                                ( getStartLocation )
import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , MemoryBlock(..)
                                                , ProgramUnitModel
                                                , SymbolTable
                                                , StorageClass(..)
                                                , StorageTable
                                                , Type(..)
                                                , SemType(..)
                                                )
import           Language.Fortran.Vars.Kind
                                                ( getTypeKind )
import           Language.Fortran.Vars.Union
                                                ( union )

-- | Given a 'SymbolTable' and an 'Expression', return the size of
-- the variable represented by the expression
getSize :: Data a => SymbolTable -> Expression (Analysis a) -> Int
getSize symTable expr =
  let symbol = case expr of
        ExpValue{} -> srcName expr
        ExpFunctionCall _ _ e@ExpValue{} _ -> srcName e
        ExpSubscript _ _ e@ExpValue{} _ -> srcName e
        _          -> error "Unsupported expression"
      Just entity = M.lookup symbol symTable
  in  case entity of
        SVariable (TArray ty dims) _ ->
          fromMaybe (error "Can't calculate size of dynamic array")
            $   sizeOfStaticArray
            <$> getTypeKind ty
            <*> dims
        SVariable ty _ ->
          fromMaybe (error "Can't get size of dynamic variable")
            $ getTypeKind ty
        _ -> error (symbol ++ " is not a VariableEntry.")

-- | Given a static array's 'kind' and 'dimension', calculate its size
sizeOfStaticArray :: Int -> [(Int, Int)] -> Int
sizeOfStaticArray kind' dimension' =
  let arraySize = foldl (\acc (l, h) -> acc * (h - l + 1)) 1 dimension'
  in  kind' * arraySize

-- | Given a 'SymbolTable', generate a 'StorageTable' for the 'SymbolTable' where
-- each symbol has been assinged to a 'MemoryBlock' within the 'StorageTable' so
-- long as it is not constant
allocateMemoryBlocks :: SymbolTable -> StorageTable
allocateMemoryBlocks = M.foldlWithKey f M.empty
 where
  f :: StorageTable -> Name -> SymbolTableEntry -> StorageTable
  f storageTable symbol entry = case entry of
    SVariable ty _ ->
      let size = case ty of
            TArray ty' dims -> sizeOfStaticArray <$> getTypeKind ty' <*> dims
            _               -> getTypeKind ty
          block = MemoryBlock
            { blockSize    = size
            , storageClass = case size of
                               Nothing -> Automatic
                               _       -> Unspecified
            , variables    = [symbol]
            }
      in  M.insert symbol block storageTable
    _ -> storageTable

-- | Given a 'ProgramUnit' and a 'ProgramUnitModel', resolve any commonly defined global
-- variables in the 'ProgramUnit's to be the same in both memory and within the 'SymbolTable'
processCommon
  :: Data a => ProgramUnit (Analysis a) -> ProgramUnitModel -> ProgramUnitModel
processCommon pu puModel =
  let commonGrps =
          [ commGrps | (StCommon _ _ (AList _ _ commGrps)) <- allPUS pu ]
      mergeCommonVariables mapping (CommonGroup _ _ commName decls) =
          let commonName = case commName of
                Just e  -> "/" ++ srcName e ++ "/"
                Nothing -> "*blank_common*"
              precedingDecls = fromMaybe [] (M.lookup commonName mapping)
          in  M.insert commonName (precedingDecls ++ aStrip decls) mapping
      commons = foldl' mergeCommonVariables M.empty (concat commonGrps)
      processComm commonName varDecls (symTable, mbs) =
          let varExps            = map declExpr varDecls
              varLocations       = map (getStartLocation symTable) varExps
              varSizes           = map (getSize symTable) varExps
              varAccumSizes      = scanl1 (+) varSizes
              commBlockLocations = map (commonName, ) (0 : varAccumSizes)
              mbs'               = case M.lookup commonName mbs of
                Just _ -> mbs
                Nothing ->
                  let newBlock = MemoryBlock { blockSize = Just $ sum varSizes
                                             , storageClass = Common
                                             , variables = []
                                             }
                  in  M.insert commonName newBlock mbs
              f model (l1, l2) = let (model', _) = union model l1 l2 in model'
          in  foldl' f (symTable, mbs') (zip commBlockLocations varLocations)
      declExpr (DeclVariable _ _ e _ _)   = e
      declExpr (DeclArray    _ _ e _ _ _) = e
  in  M.foldrWithKey processComm puModel commons
