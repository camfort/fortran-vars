module Main where

import           Language.Fortran.Extras.Encoding
                                                ( commonEncode )
import           Language.Fortran.Extras
                                                ( withProgramAnalysis )
import           Language.Fortran.Vars          ( programFileModel )

import qualified Data.ByteString.Lazy.Char8    as LB

import           Control.Monad                  ( unless )
import qualified Data.Map                      as M
                                                ( null )

programDesc, programHeader :: String
programDesc =
  "Generate symbol table and storage table from the AST of FORTRAN source"
programHeader = programDesc

main :: IO ()
main = withProgramAnalysis programDesc programHeader $ \pf -> do
    -- <String, (<String, Entry>,<String, MemoryBlock>)>
  let pfm = programFileModel pf
  unless (M.null pfm) $ LB.putStrLn $ commonEncode pfm
