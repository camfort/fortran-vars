module Main where

import           Language.Fortran.Extras.Encoding
                                                ( commonEncode )
import           Language.Fortran.Extras
                                                ( withToolOptionsAndProgramAnalysis )
import           Language.Fortran.Vars          ( programFileModel )
import           Language.Fortran.Vars.Types    ( ProgramFileModel
                                                , SymbolTableEntry(..)
                                                )
import           Language.Fortran.AST           ( ProgramUnitName(..) )

import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.Map                      as M
import           Control.Monad                  ( unless )
import           System.Environment             ( getArgs )
import           Text.Printf                    ( printf )
import           Options.Applicative

import qualified Language.Fortran.Parser as Parser

import qualified Language.Fortran.Extras.RunOptions as RO

programDesc, programHeader :: String
programDesc =
  "Generate symbol table and storage table from the AST of FORTRAN source"
programHeader = programDesc

main :: IO ()
main = do
  args <- getArgs
  withToolOptionsAndProgramAnalysis programDesc programHeader prettyFlagParser $ \prettyFlag pf -> do
    let pfm = programFileModel pf
    unless (M.null pfm) $ 
      if prettyFlag
        then putStrLn $ prettyPrintModel pfm
        else LB.putStrLn $ commonEncode pfm

prettyFlagParser :: Parser Bool
prettyFlagParser = 
   flag False True
      (  long "pretty"
      <> help "Pretty print the program file model as an ASCII table"
    )

-- | Pretty print the program file model as an ASCII table
prettyPrintModel :: ProgramFileModel -> String
prettyPrintModel pfm = unlines $ [header, separator] ++ rows ++ [separator, footer]
  where
    -- Column widths
    nameWidth = 30
    symbolsWidth = 12
    paramsWidth = 12
    varsWidth = 12
    blocksWidth = 12
    
    -- Table structure
    header = printf "| %-*s | %-*s | %-*s | %-*s | %-*s |" 
      nameWidth "Program Unit"
      symbolsWidth "Symbols"
      paramsWidth "Parameters"
      varsWidth "Variables"
      blocksWidth "Memory Blocks"
    
    separator = "+" ++ replicate (nameWidth + 2) '-' 
             ++ "+" ++ replicate (symbolsWidth + 2) '-'
             ++ "+" ++ replicate (paramsWidth + 2) '-'
             ++ "+" ++ replicate (varsWidth + 2) '-'
             ++ "+" ++ replicate (blocksWidth + 2) '-' ++ "+"
    
    rows = map formatUnit (M.toList pfm)
    
    formatUnit (punName, (symTable, storageTable)) =
      let totalSyms = M.size symTable
          params = length $ filter isParameter $ M.elems symTable
          vars = length $ filter isVariable $ M.elems symTable
          blocks = M.size storageTable
          name = showPUN punName
      in printf "| %-*s | %*d | %*d | %*d | %*d |"
           nameWidth (truncate' nameWidth name)
           (symbolsWidth - 1) totalSyms
           (paramsWidth - 1) params
           (varsWidth - 1) vars
           (blocksWidth - 1) blocks
    
    footer = printf "| %-*s | %*d | %*d | %*d | %*d |"
      nameWidth "TOTAL"
      (symbolsWidth - 1) (sum [M.size st | (st, _) <- M.elems pfm])
      (paramsWidth - 1) (sum [length $ filter isParameter $ M.elems st | (st, _) <- M.elems pfm])
      (varsWidth - 1) (sum [length $ filter isVariable $ M.elems st | (st, _) <- M.elems pfm])
      (blocksWidth - 1) (sum [M.size storage | (_, storage) <- M.elems pfm])
    
    isParameter (SParameter _ _) = True
    isParameter _ = False
    
    isVariable (SVariable _ _) = True
    isVariable _ = False
    
    truncate' n s 
      | length s <= n = s
      | otherwise = take (n - 3) s ++ "..."
    
    showPUN (Named n) = n
    showPUN (NamelessBlockData) = "<anonymous block data>"
    showPUN (NamelessComment) = "<comment>"
    showPUN (NamelessMain) = "<main>"
