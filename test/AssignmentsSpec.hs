{-# LANGUAGE LambdaCase #-}

module AssignmentsSpec where

import           Test.Hspec
import           Test.HUnit

import           Data.Either                    ( rights
                                                , partitionEithers
                                                )
import           Language.Fortran.AST           ( Expression(..)
                                                , Value(..)
                                                )
import           Language.Fortran.Extras
                                                ( allPU )
import           Language.Fortran.Extras.Test
                                                ( getTestProgramAnalysis )
import           Language.Fortran.Vars.Assignments
import           Language.Fortran.Vars.Types
                                                ( Type(..)
                                                , SemType(..)
                                                , CharacterLen(..)
                                                )

spec :: Spec
spec = describe "Grab assignment exprs" $ do
  it "All types of assignment" $ do
    pf <- getTestProgramAnalysis "test/assignment/all-assignments.f"
    let stmts = rights . concatMap allAssignStmts $ allPU pf
    length stmts `shouldBe` 4
    map fst stmts `shouldBe` [TInteger 2, TInteger 4, TInteger 8, TLogical 1]
  it "Array assignment" $ do
    pf <- getTestProgramAnalysis "test/assignment/array-assignment.f"
    let (errs, stmts) = partitionEithers . concatMap allAssignStmts $ allPU pf
    length errs `shouldBe` 0
    length stmts `shouldBe` 9
    map fst stmts `shouldBe` replicate 9 (TReal 4)
    let getVal = \case
          ExpValue _ _ (ValInteger s _) -> s
          _                           -> error "Not value"
    map (getVal . snd) stmts
      `shouldBe` ["1", "0", "0", "0", "1", "0", "0", "0", "1"]

  it "Dummy character" $ do
    pf <- getTestProgramAnalysis "test/assignment/dummy-character.f"
    let (errs, stmts) = partitionEithers . concatMap allAssignStmts $ allPU pf
    length errs `shouldBe` 0
    length stmts `shouldBe` 1
    case head stmts of
      (TCharacter CharLenStar _, ExpValue _ _ (ValString "ABCDEFGHI")) -> pure ()
      _ -> assertFailure "Wrong statement matched"
