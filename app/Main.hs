module Main where

import           Language.Fortran.Extras.Encoding
                                                ( commonEncode )
import           Language.Fortran.Extras
                                                ( withToolOptionsAndProgramAnalysis )
import           Language.Fortran.Vars          ( programFileModel )
import           Language.Fortran.Vars.PrettyPrintModel
                                                ( prettyPrintModel )

import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.Map                      as M
import           Control.Monad                  ( unless )
import           System.Environment             ( getArgs )
import           Options.Applicative            ( Parser
                                                , long
                                                , help
                                                , flag
                                                )

programDesc, programHeader :: String
programDesc =
  "Generate symbol table and storage table from the AST of FORTRAN source"
programHeader = programDesc

main :: IO ()
main = do
  _ <- getArgs
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

