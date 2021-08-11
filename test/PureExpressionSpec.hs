module PureExpressionSpec where

import           Test.Hspec

import           Language.Fortran.Extras
                                                ( allS )
import           Language.Fortran.Extras.Test
                                                ( getTestProgramAnalysis )
import           Language.Fortran.AST
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )

import           Language.Fortran.Vars.PureExpression
                                                ( isPureExpression )

-- | getRHSFunc provides a check point to test the property of an expression.
-- It grabs the right-hand side expression from an assignment statement.
-- For code like `lhsName = rhs`
-- `getRHSFunc pf "lhsName"` will return expression `rhs`
-- The lhsName shall be unique across the Program File
getRHSFunc :: ProgramFile (Analysis A0) -> Name -> Expression (Analysis A0)
getRHSFunc pf lhs = head
  [ rhs
  | (StExpressionAssign _ _ e@(ExpValue _ _ ValVariable{}) rhs) <- allS pf
  , srcName e == lhs
  ]

helper :: String -> IO (Name -> Expression (Analysis A0))
helper path = do
  pf <- getTestProgramAnalysis path
  let getRHS = getRHSFunc pf
  return getRHS

spec :: Spec
spec = describe "Pure Expression" $ do

  let path = "test/pure_expression.f"
  it "Constant and Vairable" $ do
    getRHS <- helper path

    isPureExpression (getRHS "i1") `shouldBe` True
    isPureExpression (getRHS "i2") `shouldBe` True

  it "Unary/Binary Operations" $ do
    getRHS <- helper path
    isPureExpression (getRHS "i3") `shouldBe` True
    isPureExpression (getRHS "i4") `shouldBe` True
    isPureExpression (getRHS "b1") `shouldBe` True
    isPureExpression (getRHS "b2") `shouldBe` True

  it "Array" $ do
    getRHS <- helper path
    isPureExpression (getRHS "i4") `shouldBe` True
    isPureExpression (getRHS "i5") `shouldBe` True

  it "Exnternal Function" $ do
    getRHS <- helper path
    isPureExpression (getRHS "i6") `shouldBe` False
    isPureExpression (getRHS "i7") `shouldBe` False
    isPureExpression (getRHS "i8") `shouldBe` False
    isPureExpression (getRHS "i9") `shouldBe` False
    isPureExpression (getRHS "b3") `shouldBe` False

  it "Intrinsic Function" $ do
    getRHS <- helper path
    isPureExpression (getRHS "r1") `shouldBe` True
    isPureExpression (getRHS "i10") `shouldBe` True
    isPureExpression (getRHS "i11") `shouldBe` False
