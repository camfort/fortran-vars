module Language.Fortran.Vars.StorageClass
  ( processStorageClass
  )
where

import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( aStrip
                                                , Declarator(..)
                                                , Expression(..)
                                                , Name
                                                , Statement(..)
                                                , Value(..)
                                                )

import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , MemoryBlock(..)
                                                , ProgramUnitModel
                                                , StorageClass(..)
                                                )

storageClassStmt
  :: Data a => ProgramUnitModel -> Statement (Analysis a) -> ProgramUnitModel
storageClassStmt puModel (StAutomatic _ _ decls) = foldl' f
                                                          puModel
                                                          (aStrip decls)
 where
  f m (Declarator _ _ varExp _ _ _) =
    updateStorageClass (srcName varExp) Automatic m
storageClassStmt puModel (StSave _ _ (Just exps)) = foldl' f
                                                           puModel
                                                           (aStrip exps)
 where
  f m e@(ExpValue _ _ (ValVariable _)) =
    updateStorageClass (srcName e) Static m
  f m _ = m
storageClassStmt puModel _ = puModel

updateStorageClass
  :: Name -> StorageClass -> ProgramUnitModel -> ProgramUnitModel
updateStorageClass symbol stClass m@(symTable, storageTable) =
  case M.lookup symbol symTable of
    Just (SVariable _ (blockName, _)) ->
      case M.lookup blockName storageTable of
        Just block | storageClass block /= stClass ->
          let blk = block { storageClass = stClass }
          in  (symTable, M.insert blockName blk storageTable)
        _ -> m
    _ -> m

-- | Given all of the 'Statement's in a program as well as a 'ProgramUnitModel', produce a new
-- 'ProgramUnitModel' where the 'StorageClass's of each symbol have been determined
processStorageClass
  :: Data a => [Statement (Analysis a)] -> ProgramUnitModel -> ProgramUnitModel
processStorageClass stmts puModel0 = foldl' storageClassStmt puModel0 stmts
