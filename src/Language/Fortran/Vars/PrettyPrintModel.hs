-- | Pretty printing utilities for Fortran variable analysis results

module Language.Fortran.Vars.PrettyPrintModel
  ( prettyPrintModel
  ) where

import           Language.Fortran.Vars.Types    ( ProgramFileModel
                                                , SymbolTableEntry(..)
                                                , MemoryBlock(..)
                                                , StorageTable
                                                , SymbolTable
                                                )
import           Language.Fortran.Vars.Types.SymbolTable ( MemoryBlockName )
import           Language.Fortran.AST           ( ProgramUnitName(..) )
import           Language.Fortran.Vars.Rep      ( Type
                                                , ExpVal(..)
                                                )
import           Language.Fortran.PrettyPrint   ( pprintAndRender )
import           Language.Fortran.Version       ( FortranVersion(..) )

import qualified Data.Map                      as M
import           Text.Printf                    ( printf )

-- | Pretty print the program file model as an ASCII table showing detailed information
prettyPrintModel :: ProgramFileModel -> String
prettyPrintModel pfm = unlines $ concatMap formatProgramUnit (M.toList pfm)
  where
    formatProgramUnit :: (ProgramUnitName, (SymbolTable, StorageTable)) -> [String]
    formatProgramUnit (punName, (symTable, storageTable)) =
      let unitName = showPUN punName
          unitHeader = "\n" <> replicate 100 '=' <> "\n" 
                    <> "Program Unit: " <> unitName <> "\n"
                    <> replicate 100 '='
          combinedTable = if M.null symTable
                          then "\n(No symbols)"
                          else formatCombinedTable symTable storageTable
      in [unitHeader, combinedTable]

    formatCombinedTable :: SymbolTable -> StorageTable -> String
    formatCombinedTable symTable storageTable = 
      let tableHeader = "\n\n" <> combinedSep <> "\n" <> combinedHeader <> "\n" <> combinedSep
          symRows = map (formatSymbol storageTable) (M.toList symTable)
      in unlines (tableHeader : symRows <> [combinedSep])
    
    -- Column widths for combined table
    nameW = 20
    tagW = 10
    typeW = 30
    blockW = 15
    offsetW = 8
    sizeW = 10
    classW = 12
    
    combinedHeader = printf "| %-*s | %-*s | %-*s | %-*s | %-*s | %-*s | %-*s |"
      nameW "Name" tagW "Tag" typeW "Type" blockW "Block/Value" offsetW "Offset" sizeW "Size" classW "Class"
    
    combinedSep = "+" <> replicate (nameW + 2) '-'
               <> "+" <> replicate (tagW + 2) '-'
               <> "+" <> replicate (typeW + 2) '-'
               <> "+" <> replicate (blockW + 2) '-'
               <> "+" <> replicate (offsetW + 2) '-'
               <> "+" <> replicate (sizeW + 2) '-'
               <> "+" <> replicate (classW + 2) '-' <> "+"
    
    formatSymbol :: M.Map MemoryBlockName MemoryBlock -> (String, SymbolTableEntry) -> String
    formatSymbol storageTable (name, entry) = case entry of
      SParameter ty val -> 
        printf "| %-*s | %-*s | %-*s | %-*s | %-*s | %-*s | %-*s |"
          nameW (trunc nameW name)
          tagW "Parameter"
          typeW (trunc typeW $ showType ty)
          blockW (trunc blockW $ showExpVal val)
          offsetW ""
          sizeW ""
          classW "Constant"
      
      SVariable ty (blockName, offset) ->
        let (sizeStr, classStr) = case M.lookup blockName storageTable of
              Nothing -> ("", "")
              Just memBlock -> 
                let size = case blockSize memBlock of
                      Just s -> show s
                      Nothing -> "dynamic"
                    cls = show $ storageClass memBlock
                in (size, cls)
        in printf "| %-*s | %-*s | %-*s | %-*s | %-*d | %-*s | %-*s |"
             nameW (trunc nameW name)
             tagW "Variable"
             typeW  (trunc typeW $ showType ty)
             blockW (trunc blockW blockName)
             offsetW offset
             sizeW  (trunc sizeW sizeStr)
             classW (trunc classW classStr)
      
      SDummy ty ->
        printf "| %-*s | %-*s | %-*s | %-*s | %-*s | %-*s | %-*s |"
          nameW (trunc nameW name)
          tagW "Dummy"
          typeW (trunc typeW $ showType ty)
          blockW ""
          offsetW ""
          sizeW ""
          classW ""
      
      SExternal ty ->
        printf "| %-*s | %-*s | %-*s | %-*s | %-*s | %-*s | %-*s |"
          nameW (trunc nameW name)
          tagW "External"
          typeW (trunc typeW $ showType ty)
          blockW ""
          offsetW ""
          sizeW ""
          classW ""
    
    trunc n s 
      | length s <= n = s
      | otherwise = take (n - 3) s <> "..."
    
    showPUN (Named n) = n
    showPUN (NamelessBlockData) = "<anonymous block data>"
    showPUN (NamelessComment) = "<comment>"
    showPUN (NamelessMain) = "<main>"
    
    showExpVal (Int i) = show i
    showExpVal (Real d) = show d
    showExpVal (Str s) = show s
    showExpVal (Logical b) = show b
    showExpVal (Boz b) = show b

-- | Format a type for display using fortran-src's pretty printer
showType :: Type -> String
showType ty = pprintAndRender Fortran90 ty (Just 0)
