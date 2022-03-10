module TypeCheckSpec where

import           Test.Hspec
import           Control.Monad                  ( zipWithM_ )
import           Data.Either                    ( isLeft )
import           Data.List                      ( find )
import qualified Data.Map                      as M
import           Data.Data                      ( Data )
import           Language.Fortran.Extras        ( allPUS
                                                , allPU
                                                )
import           Language.Fortran.Extras.Test   ( getTestProgramAnalysis )
import           Language.Fortran.AST
import           Language.Fortran.Analysis      ( stripAnalysis )
import           Language.Fortran.Vars          ( programFileModel )
import           Language.Fortran.Vars.Types    ( Type
                                                , SemType(..)
                                                , CharacterLen(..)
                                                )
import           Language.Fortran.Vars.TypeCheck
                                                ( typeOf
                                                , TypeOf
                                                , TypeError(..)
                                                )
import           Language.Fortran.Vars.SymbolTable
                                                ( collectSymbols )
import           Language.Fortran.Vars.StructureTable
                                                ( collectStructures )

-- | Given a varaible name, 'RHSFunc' search assignment statements within a program
-- unit and returns the RHS of first assignment statement whose LHS matches the
-- input variable name.
type RHSFunc a = Name -> Expression a

-- | Given a 'ProgramUnit', returns a 'RHSFunc' function that can be used to
-- search the 'ProgramFile' and return the RHS of an assignment.
getRhsFunc :: Data a => ProgramUnit a -> RHSFunc a
getRhsFunc pu lhs = head
  [ rhs
  | (StExpressionAssign _ _ (ExpValue _ _ (ValVariable name)) rhs) <- allPUS pu
  , name == lhs
  ]


helper :: String -> Name -> IO (TypeOf A0, RHSFunc A0)
helper path unit = do
  pf <- getTestProgramAnalysis path
  let pfm                = programFileModel pf
      Just (symTable, _) = M.lookup (Named unit) pfm
      strTable           = collectStructures symTable pu
      typeof             = typeOf strTable symTable
      Just pu            = find (\x -> getName x == Named unit) (allPU pf)
      rhs                = getRhsFunc (stripAnalysis pu)
  return (typeof, rhs)

unknownType :: Either TypeError Type -> Bool
unknownType (Left (UnknownType _)) = True
unknownType _                      = False

typeError :: Either TypeError Type -> Bool
typeError (Left TypeError{}) = True
typeError _                  = False

unboundVariable :: Either TypeError Type -> Bool
unboundVariable (Left (UnboundVariable _)) = True
unboundVariable _                          = False

spec :: Spec
spec = do

  describe "Constants" $ do

    let path   = "test/type_check/constants.f"
    let puName = "constants"
    it "Integer Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "i1") `shouldBe` Right (TInteger 4)
      typeof (rhs "i2") `shouldBe` Right (TInteger 4)
      typeof (rhs "i3") `shouldBe` Right (TInteger 4)
      typeof (rhs "i4") `shouldBe` Right (TInteger 4)


    it "Logical Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "l1") `shouldBe` Right (TLogical 4)
      typeof (rhs "l2") `shouldBe` Right (TLogical 4)

    it "Real Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "r1") `shouldBe` Right (TReal 4)
      typeof (rhs "r2") `shouldBe` Right (TReal 4)
      typeof (rhs "r3") `shouldBe` Right (TReal 4)
      typeof (rhs "r4") `shouldBe` Right (TReal 4)
      typeof (rhs "r5") `shouldBe` Right (TReal 4)

      typeof (rhs "r6") `shouldBe` Right (TReal 8)
      typeof (rhs "r7") `shouldBe` Right (TReal 8)

    it "Complex Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "c1") `shouldBe` Right (TComplex 8)
      typeof (rhs "c2") `shouldBe` Right (TComplex 8)
      typeof (rhs "c3") `shouldBe` Right (TComplex 8)
      typeof (rhs "c4") `shouldBe` Right (TComplex 8)

      typeof (rhs "c5") `shouldBe` Right (TComplex 16)
      typeof (rhs "c6") `shouldBe` Right (TComplex 16)
      typeof (rhs "c7") `shouldBe` Right (TComplex 16)

    it "Character Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "s1") `shouldBe` Right (TCharacter (CharLenInt 8) 1)

    it "BOZ Constant" $ do
      (typeof, rhs) <- helper path puName
      mapM_ (\v -> typeof (rhs v) `shouldBe` Right (TByte 4))
            ["b1", "b2", "b3", "b4"]

    it "Hollerith Constant" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "h1") `shouldBe` Right (TByte 2)
      typeof (rhs "h2") `shouldBe` Right (TByte 8)

  describe "Variables" $ do
    let path   = "test/type_check/variables.f"
        puName = "variables"

    it "Byte" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b") `shouldBe` Right (TByte 1)

    it "Character" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "c") `shouldBe` Right (TCharacter (CharLenInt 1) 1)
      typeof (rhs "c1") `shouldBe` Right (TCharacter (CharLenInt 7) 1)
      typeof (rhs "c2") `shouldBe` Right (TCharacter (CharLenInt 7) 1)
      typeof (rhs "c3") `shouldBe` Right (TCharacter (CharLenInt 4) 1)
      typeof (rhs "c4") `shouldBe` Right (TCharacter (CharLenInt 8) 1)

    it "Integer" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "i") `shouldBe` Right (TInteger 4)
      typeof (rhs "i2") `shouldBe` Right (TInteger 2)
      typeof (rhs "i4") `shouldBe` Right (TInteger 4)
      typeof (rhs "i8") `shouldBe` Right (TInteger 8)
      typeof (rhs "i2ns") `shouldBe` Right (TInteger 2)
      typeof (rhs "i4ns") `shouldBe` Right (TInteger 4)
      typeof (rhs "i8ns") `shouldBe` Right (TInteger 8)

    it "Real" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "r") `shouldBe` Right (TReal 4)
      typeof (rhs "r4") `shouldBe` Right (TReal 4)
      typeof (rhs "r8") `shouldBe` Right (TReal 8)
      typeof (rhs "r4ns") `shouldBe` Right (TReal 4)
      typeof (rhs "r8ns") `shouldBe` Right (TReal 8)
      typeof (rhs "dp") `shouldBe` Right (TReal 8)

  describe "Array" $ do
    let path   = "test/type_check/array_and_substring.f"
        puName = "array"

    it "Array" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "arr1") `shouldBe` Right (TInteger 4)
      typeof (rhs "arr2") `shouldBe` Right (TInteger 4)

    it "Adjustable Array" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "arr3") `shouldBe` Right (TInteger 4)

    it "Assumed-size Array" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "arr4") `shouldBe` Right (TInteger 4)

    it "Array partial access" $ do
      -- implicit lower bound arrays
      (typeof, rhs) <- helper path puName
      typeof (rhs "arr5") `shouldBe` Right (TInteger 4)

    it "Index ranges" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "i1") `shouldBe` Right (TArray (TInteger 4) (Just [(1, 10)]))
      typeof (rhs "i2") `shouldBe` Right (TArray (TInteger 4) (Just [(1, 10)]))
      typeof (rhs "i3") `shouldBe` Right (TArray (TInteger 4) Nothing)

    it "Erroneous expressions" $ do
      -- These expressions aren't valid but any subscript can be assumed to
      -- access a scalar element from it
      (typeof, rhs) <- helper path puName
      typeof (rhs "err1") `shouldBe` Right (TInteger 4)
      typeof (rhs "err2") `shouldBe` Right (TInteger 4)
      typeof (rhs "err3") `shouldBe` Right (TInteger 4)
      -- Can't deal with array sections
      isLeft (typeof (rhs "err4")) `shouldBe` True

  describe "String" $ do
    let path   = "test/type_check/array_and_substring.f"
        puName = "string"

    it "Assumed-size string" $ do
      -- TODO all Nothings, unsure though what assumed-size means
      (typeof, rhs) <- helper path puName
      typeof (rhs "s1") `shouldBe` Right (TCharacter CharLenStar 1)
      typeof (rhs "s2") `shouldBe` Right (TCharacter CharLenStar 1)
      typeof (rhs "s3") `shouldBe` Right (TCharacter CharLenStar 1)

    it "substring" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "s4") `shouldBe` Right (TCharacter (CharLenInt 5) 1)
      typeof (rhs "s5") `shouldBe` Right (TCharacter (CharLenInt 6) 1)
      typeof (rhs "s6") `shouldBe` Right (TCharacter (CharLenInt 10) 1)
      typeof (rhs "s7") `shouldBe` Right (TCharacter (CharLenInt 3) 1)
      -- Dynamic arrays
      typeof (rhs "s8") `shouldBe` Right (TCharacter CharLenStar 1)
      typeof (rhs "s9") `shouldBe` Right (TCharacter CharLenStar 1)
      typeof (rhs "s10") `shouldBe` Right (TCharacter CharLenStar 1)

  describe "Expression" $ do
    let path   = "test/type_check/expression.f"
        puName = "expression"

    it "Unary Expression" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "u1") `shouldBe` Right (TInteger 4)
      typeof (rhs "u2") `shouldBe` Right (TInteger 4)
      typeof (rhs "u3") `shouldBe` Right (TReal 4)
      typeof (rhs "u4") `shouldBe` Right (TReal 4)
      typeof (rhs "u5") `shouldBe` Right (TLogical 4)
      typeof (rhs "u6") `shouldBe` Right (TLogical 4)

    it "Arithmetic Expression with Type Promotion" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "a1") `shouldBe` Right (TInteger 4)
      typeof (rhs "a2") `shouldBe` Right (TInteger 4)
      typeof (rhs "a3") `shouldBe` Right (TInteger 8)
      typeof (rhs "a4") `shouldBe` Right (TInteger 8)
      typeof (rhs "a5") `shouldBe` Right (TInteger 8)

      typeof (rhs "a6") `shouldBe` Right (TInteger 4)
      typeof (rhs "a7") `shouldBe` Right (TInteger 4)
      typeof (rhs "a8") `shouldBe` Right (TInteger 4)
      typeof (rhs "a9") `shouldBe` Right (TInteger 8)

      typeof (rhs "a10") `shouldBe` Right (TReal 4)
      typeof (rhs "a11") `shouldBe` Right (TReal 8)
      typeof (rhs "a12") `shouldBe` Right (TReal 8)
      typeof (rhs "a13") `shouldBe` Right (TReal 8)
      typeof (rhs "a14") `shouldBe` Right (TReal 8)
      typeof (rhs "a15") `shouldBe` Right (TReal 8)
      typeof (rhs "a16") `shouldBe` Right (TReal 8)
      typeof (rhs "a17") `shouldBe` Right (TReal 4)

      typeof (rhs "a18") `shouldBe` Right (TComplex 8)
      typeof (rhs "a19") `shouldBe` Right (TComplex 8)
      typeof (rhs "a20") `shouldBe` Right (TComplex 16)
      typeof (rhs "a21") `shouldBe` Right (TComplex 16)

    it "Character Concatenation" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "s1") `shouldBe` Right (TCharacter (CharLenInt 8) 1)
      typeof (rhs "s1") `shouldBe` Right (TCharacter (CharLenInt 8) 1)

    it "Relational Expression" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "re1") `shouldBe` Right (TLogical 4)
      typeof (rhs "re2") `shouldBe` Right (TLogical 4)
      typeof (rhs "re3") `shouldBe` Right (TLogical 4)
      typeof (rhs "re4") `shouldBe` Right (TLogical 4)
      typeof (rhs "re5") `shouldBe` Right (TLogical 4)
      typeof (rhs "re6") `shouldBe` Right (TLogical 4)

    it "Logical Expression" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "le1") `shouldBe` Right (TLogical 4)
      typeof (rhs "le2") `shouldBe` Right (TLogical 8)
      typeof (rhs "le3") `shouldBe` Right (TLogical 2)
      typeof (rhs "le4") `shouldBe` Right (TLogical 2)
      typeof (rhs "le5") `shouldBe` Right (TLogical 4)

    it "More expressions" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "e1") `shouldBe` Right (TInteger 8)
      typeof (rhs "e2") `shouldBe` Right (TInteger 4)
      typeof (rhs "e3") `shouldBe` Right (TComplex 8)
      typeof (rhs "e4") `shouldBe` Right (TLogical 4)
      typeof (rhs "e5") `shouldBe` Right (TInteger 4)

  describe "Intrinsics" $ do
    let path   = "test/type_check/intrinsic.f"
        puName = "intrinsictest"

    it "Integer intrinsic" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b1") `shouldBe` Right (TInteger 4)
      typeof (rhs "b2") `shouldBe` Right (TInteger 4)

    it "Real intrinsic" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b3") `shouldBe` Right (TReal 4)
      typeof (rhs "b4") `shouldBe` Right (TReal 4)

    it "Character intrinsic" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b5") `shouldBe` Right (TCharacter (CharLenInt 1) 1)

    it "Generic intrinsic" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b6") `shouldBe` Right (TInteger 4)
      typeof (rhs "b7") `shouldBe` Right (TReal 4)
      typeof (rhs "b8") `shouldBe` Right (TInteger 8)
      typeof (rhs "b9") `shouldBe` Right (TReal 8)

    it "INT(e), INT(e, k), and INT2" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b10") `shouldBe` Right (TInteger 4)
      typeof (rhs "b11") `shouldBe` Right (TInteger 2)
      typeof (rhs "b12") `shouldBe` Right (TInteger 4)
      typeof (rhs "b13") `shouldBe` Right (TInteger 8)
      typeof (rhs "b14") `shouldBe` Right (TInteger 8)
      typeof (rhs "b15") `shouldBe` Right (TInteger 2)
      typeof (rhs "b16") `shouldBe` Right (TInteger 2)
      typeof (rhs "b17") `shouldBe` Right (TInteger 2)

    it "iand, ior, ishft" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b18") `shouldBe` Right (TInteger 4)
      typeof (rhs "b19") `shouldBe` Right (TInteger 4)
      typeof (rhs "b20") `shouldBe` Right (TInteger 8)
      typeof (rhs "b21") `shouldBe` Right (TInteger 4)
      typeof (rhs "b22") `shouldBe` Right (TInteger 4)
      typeof (rhs "b23") `shouldBe` Right (TInteger 8)
      typeof (rhs "b24") `shouldBe` Right (TInteger 4)
      typeof (rhs "b25") `shouldBe` Right (TInteger 8)
      typeof (rhs "b26") `shouldBe` Right (TInteger 2)

    it "Misc intrinsics" $ do
      (typeof, rhs) <- helper path puName
      typeof (rhs "b27") `shouldBe` Right (TReal 8)
      typeof (rhs "b28") `shouldBe` Right (TInteger 4)
      typeof (rhs "b29") `shouldBe` Right (TInteger 4)
      typeof (rhs "b30") `shouldBe` Right (TInteger 4)
      typeof (rhs "b31") `shouldBe` Right (TInteger 4)
      typeof (rhs "b32") `shouldBe` Right (TInteger 4)
      typeof (rhs "b33") `shouldBe` Right (TInteger 4)
      typeof (rhs "b34") `shouldBe` Right (TInteger 8)
      typeof (rhs "b35") `shouldBe` Right (TInteger 4)
      typeof (rhs "b36") `shouldBe` Right (TInteger 4)
      typeof (rhs "b37") `shouldBe` Right (TInteger 8)
      typeof (rhs "b38") `shouldBe` Right (TLogical 4)
      typeof (rhs "b39") `shouldBe` Right (TReal 8)
      typeof (rhs "b40") `shouldBe` Right (TInteger 8)

  describe "Implied Do" $ it "data statements" $ do
    ProgramFile _ (pu : _) <- getTestProgramAnalysis
      "test/type_check/implied_do.f"
    let
      symt = collectSymbols pu
      strt = collectStructures symt pu
      dgs  = [ aStrip dgs' | StData _ _ dgs' <- allPUS pu ]
      test (DataGroup _ _ es _ : _) dims = typeOf strt symt (head $ aStrip es)
        `shouldBe` Right (TArray (TInteger 2) (Just dims))
      test _ _ = error "Shouldn't reach this"
    let res =
          [ [(1, 5)]
          , [(1, 3)]
          , [(1, 3)]
          , [(1, 9)]
          , [(1, 2)]
          , [(1, 6)]
          , [(1, 3)]
          , [(1, 2)]
          , [(1, 2)]
          ]
    length dgs `shouldBe` length res
    zipWithM_ test dgs res
