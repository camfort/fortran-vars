module Language.Fortran.Vars
  ( programUnitModel
  , programFileModel
  )
where

import           Language.Fortran.Extras
                                                ( allPUS )
import           Data.Data                      ( Data )
import           Data.Function                  ( (&) )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , puName
                                                )
import           Language.Fortran.AST           ( ProgramFile(..)
                                                , ProgramUnit(..)
                                                )

import           Language.Fortran.Vars.Dummy
                                                ( undefineDummyArguments )
import           Language.Fortran.Vars.Equivalence
                                                ( processEquivalence )
import           Language.Fortran.Vars.Memory
                                                ( allocateMemoryBlocks
                                                , processCommon
                                                )
import           Language.Fortran.Vars.StorageClass
                                                ( processStorageClass )
import           Language.Fortran.Vars.SymbolTable
                                                ( collectSymbols )
import           Language.Fortran.Vars.Types
                                                ( ProgramFileModel
                                                , ProgramUnitModel
                                                )

-- | Given a 'ProgramUnit', generate a 'ProgramUnitModel' that contains not only a
-- 'SymbolTable' for the non-intrinsic symbols in the unit, but also a
-- 'Language.Fortran.Vars.Types.StorageTable' that determines the locations
-- that non-constant, non-parameter variables will be allocated
programUnitModel :: Data a => ProgramUnit (Analysis a) -> ProgramUnitModel
programUnitModel pu =
  let stmts     = allPUS pu
      symTable1 = undefineDummyArguments pu . collectSymbols $ pu
      mbs1      = allocateMemoryBlocks symTable1
  in  (symTable1, mbs1)
        & processStorageClass stmts
        & processCommon pu
        & processEquivalence stmts

-- | Given a 'ProgramFile', generate a 'ProgramFileModel' for each 'ProgramUnit' in
-- the file that contains not only a 'SymbolTable' for the non-intrinsic symbols in
-- the unit, but also a 'Language.Fortran.Vars.Types.StorageTable' that
-- determines the locations that non-constant, non-parameter variables will be allocated
programFileModel :: Data a => ProgramFile (Analysis a) -> ProgramFileModel
programFileModel (ProgramFile _ pus) = foldl' handler M.empty pus
  where handler m pu = M.insert (puName pu) (programUnitModel pu) m
