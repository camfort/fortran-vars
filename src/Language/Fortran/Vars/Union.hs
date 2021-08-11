module Language.Fortran.Vars.Union
  ( union
  )
where

import qualified Data.Map                      as M
import           Language.Fortran.AST           ( Name )

import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , Location
                                                , MemoryBlock(..)
                                                , MemoryBlockName
                                                , ProgramUnitModel
                                                , StorageClass(..)
                                                , SymbolTable
                                                )

updateVal :: MemoryBlockName -> Int -> Name -> SymbolTable -> SymbolTable
updateVal blockName diff symbol symt = case M.lookup symbol symt of
  Just (SVariable ty (_, offset)) ->
    let entry = SVariable ty (blockName, offset + diff)
    in  M.insert symbol entry symt
  Just _  -> error (symbol ++ "is not a variable.")
  Nothing -> symt

mergeStClass :: StorageClass -> StorageClass -> StorageClass
mergeStClass Unspecified c2          = c2
mergeStClass c1          Unspecified = c1
mergeStClass c1 c2 | c1 == c2        = c1
mergeStClass c1 c2 =
  error ("Try to merge StorageClass " ++ show c1 ++ " with " ++ show c2)

-- | Given a 'ProgramUnitModel' and two different 'Location's,
-- produce a new 'ProgramUnitModel' and 'Location' that represents
-- the union of the inputs
union
  :: ProgramUnitModel -> Location -> Location -> (ProgramUnitModel, Location)
union puModel loc1 loc2 | loc1 == loc2 = (puModel, loc1)
union (symTable, storageTable) location1 location2 =
  let (blockName1, offset1) = location1
      (blockName2, offset2) = location2
      Just block1           = case M.lookup blockName1 storageTable of
        Just block -> Just block
        Nothing    -> error ("Block doesn't exist: " ++ blockName1)
      Just block2 = case M.lookup blockName2 storageTable of
        Just block -> Just block
        Nothing    -> error ("Block doesn't exist: " ++ blockName2)
  in  case compare offset1 offset2 of
        Prelude.GT -> mergeTo location1 location2
        Prelude.LT -> mergeTo location2 location1
        Prelude.EQ -> case (storageClass block1, storageClass block2) of
          (Common, _     ) -> mergeTo location1 location2
          (_     , Common) -> mergeTo location2 location1
          _                -> if blockSize block1 >= blockSize block2
            then mergeTo location1 location2
            else mergeTo location2 location1
 where
  mergeTo :: Location -> Location -> (ProgramUnitModel, Location)
  mergeTo toLocation@(toBlockName, toOffset) (fromBlockName, fromOffset) =
    let
      diff           = toOffset - fromOffset
      Just fromBlock = M.lookup fromBlockName storageTable
      Just toBlock   = M.lookup toBlockName storageTable
      -- update toBlock variables by appending the variables of fromBlock
      newVarList     = variables toBlock ++ variables fromBlock
      -- update toBlock size
      newSize        = do
        to   <- blockSize toBlock
        from <- blockSize fromBlock
        pure $ max to (from + diff)
      newStClass = mergeStClass (storageClass fromBlock) (storageClass toBlock)
      newBlock   = MemoryBlock { blockSize    = newSize
                               , storageClass = newStClass
                               , variables    = newVarList
                               }
      mbs  = M.insert toBlockName newBlock storageTable
      -- remove the fromBlock
      mbs' = M.delete fromBlockName mbs
      -- update the symbolTable for variables of fromBlock with
      -- toBlock name and updated offset
      symTable' =
        foldr (updateVal toBlockName diff) symTable (variables fromBlock)
    in
      ((symTable', mbs'), toLocation)
