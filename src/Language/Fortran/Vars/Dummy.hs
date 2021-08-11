module Language.Fortran.Vars.Dummy
  ( undefineDummyArguments
  )
where

import           Language.Fortran.Extras
                                                ( allPUS )
import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList
                                                , aStrip
                                                , Value(..)
                                                , Expression(..)
                                                , ProgramUnit(..)
                                                , Statement(..)
                                                )

import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , SymbolTable
                                                )

dummyArgInProcedure
  :: Data a => SymbolTable -> ProgramUnit (Analysis a) -> SymbolTable
dummyArgInProcedure symt (PUSubroutine _ _ _ _ args _ _) = dummyArgs symt args
dummyArgInProcedure symt (PUFunction _ _ _ _ _ args _ _ _) =
  dummyArgs symt args
dummyArgInProcedure symt _ = symt

dummyArgInStmtFunction
  :: Data a => SymbolTable -> Statement (Analysis a) -> SymbolTable
dummyArgInStmtFunction symt (StFunction _ _ _ args _) =
  dummyArgs symt (Just args)
dummyArgInStmtFunction symt _ = symt

dummyArgs
  :: Data a
  => SymbolTable
  -> Maybe (AList Expression (Analysis a))
  -> SymbolTable
dummyArgs symt maybeArgs = case maybeArgs of
  Just args -> foldl' undefineDummyArg symt (aStrip args)
  Nothing   -> symt

undefineDummyArg
  :: Data a => SymbolTable -> Expression (Analysis a) -> SymbolTable
undefineDummyArg symt (ExpValue _ _ ValStar) = symt
undefineDummyArg symt varExp =
  let symbol = srcName varExp
  in  case M.lookup symbol symt of
        Just ve@SVariable{} -> M.insert symbol (variableToDummy ve) symt
        Just SParameter{} ->
          error (symbol ++ "is a parameter, invalid fortran syntax.")
        _ -> symt

variableToDummy :: SymbolTableEntry -> SymbolTableEntry
variableToDummy (SVariable ty _) = SDummy ty
variableToDummy _ =
  error "Only VariableEntry might be transformed to DummyEntry."

-- | Since FORTRAN parameters to functions can only have their memory allocation determined
-- at runtime, given a 'ProgramUnit' and a 'SymbolTable', return a new 'SymbolTable' where
-- all of the parameters have their 'DenotedVal' set to 'Undefined'
undefineDummyArguments
  :: Data a => ProgramUnit (Analysis a) -> SymbolTable -> SymbolTable
undefineDummyArguments pu symTable =
  let symTable1 = dummyArgInProcedure symTable pu
  in  foldl' dummyArgInStmtFunction symTable1 $ allPUS pu
