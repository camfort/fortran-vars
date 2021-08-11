module Language.Fortran.Vars.CommonLayout
  ( getCommonLayout
  , getFlagType
  )
where

import qualified Data.Map                      as M
import           Language.Fortran.AST           ( Name )
import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , MemoryBlock(..)
                                                , ProgramUnitModel
                                                , StorageClass(..)
                                                , Offset
                                                , Type(..)
                                                , SemType(..)
                                                , CharacterLen(..)
                                                )

data FlagType =
  Default
  | AlignCommons
  | NoAlignCommons

getFlagType :: String -> FlagType
getFlagType ""                  = Default
getFlagType "falign-commons"    = AlignCommons
getFlagType "fno-align-commons" = NoAlignCommons
getFlagType _                   = Default

getCommonLayout
  :: ProgramUnitModel -> String -> FlagType -> [(Name, Offset, Type)]
getCommonLayout (symbolTable, storageTable) commonArea flagOptions =
  generateOffset annotatedVariables
 where
  annotatedVariables =
    map (\x -> (x, getOffset x, getType x)) (getVariables commonArea)
  generateOffset list = case flagOptions of
    Default        -> list
    NoAlignCommons -> list
    AlignCommons   -> getPaddedOffset list 0
  getPaddedOffset [] _ = []
  getPaddedOffset ((name, offset, variableType) : xs) cumm =
    (name, offset + newCumm, variableType) : getPaddedOffset xs newCumm
   where
    newCumm = if diff > 0 then cumm + size - diff else cumm
    diff    = (offset + cumm) `mod` size
    size    = getSize variableType
  getSize variable = case variable of
    TInteger   size        -> size
    TReal      size        -> size
    TComplex   size        -> size
    TLogical   size        -> size
    TByte      size        -> size
    TCharacter (CharLenInt i) k -> i*k
    TCharacter _ _ ->
      error "Cannot handle dynamic length TCharacter in common area"
    TArray innerType _ -> getSize innerType
    TCustom _          -> error "Cannot handle TCustom in common area"
  getVariables cmn =
    let cmnStorageName = "/" ++ cmn ++ "/"
    in  case M.lookup cmnStorageName storageTable of
          Just memoryBlock | storageClass memoryBlock == Common ->
            variables memoryBlock
          _ -> []
  getOffset variableName = case M.lookup variableName symbolTable of
    Just (SVariable _ (_, offset)) -> offset
    _                              -> error "variable not found in symbolTable"
  getType variableName = case M.lookup variableName symbolTable of
    Just (SVariable variableType _) -> variableType
    _                               -> error "variable not found in symbolTable"
