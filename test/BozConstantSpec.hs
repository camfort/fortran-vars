-- TODO 2022-08-22 raehik: essentially obsoleted by
-- @Language.Fortran.AST.Literal.BozSpec@ in fortran-src

module BozConstantSpec
  ( spec
  )
where

import           Test.Hspec

import           Language.Fortran.Vars.BozConstant
                                                ( bozToInt2
                                                , bozToInt4
                                                , bozToInt8
                                                )
import           Language.Fortran.Vars.Types    ( ExpVal(..) )
import qualified Language.Fortran.AST.Literal.Boz      as AST

boz :: String -> AST.Boz
boz = AST.parseBoz

spec :: Spec
spec = describe "Boz Constant Conversion" $ do
  it "Mixed case BOZ constant conversions" $ do
    bozToInt2 (boz "'ffff'x") `shouldBe` Int (-1)
    bozToInt2 (boz "'FfFf'x") `shouldBe` Int (-1)
    bozToInt4 (boz "'FFFFFFFF'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*2" $ do
    bozToInt2 (boz "'1'x") `shouldBe` Int 1
    bozToInt2 (boz "'10'x") `shouldBe` Int 16
    bozToInt2 (boz "'7fff'x") `shouldBe` Int 32767
    bozToInt2 (boz "'8000'x") `shouldBe` Int (-32768)
    bozToInt2 (boz "'ffff'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*4" $ do
    bozToInt4 (boz "'1'x") `shouldBe` Int 1
    bozToInt4 (boz "'10'x") `shouldBe` Int 16
    bozToInt4 (boz "'7fff'x") `shouldBe` Int 32767
    bozToInt4 (boz "'8000'x") `shouldBe` Int 32768
    bozToInt4 (boz "'ffff'x") `shouldBe` Int 65535
    bozToInt4 (boz "'7fffffff'x") `shouldBe` Int 2147483647
    bozToInt4 (boz "'80000000'x") `shouldBe` Int (-2147483648)
    bozToInt4 (boz "'ffffffff'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*8" $ do
    bozToInt8 (boz "'1'x") `shouldBe` Int 1
    bozToInt8 (boz "'10'x") `shouldBe` Int 16
    bozToInt8 (boz "'7fff'x") `shouldBe` Int 32767
    bozToInt8 (boz "'8000'x") `shouldBe` Int 32768
    bozToInt8 (boz "'ffff'x") `shouldBe` Int 65535
    bozToInt8 (boz "'7fffffff'x") `shouldBe` Int 2147483647
    bozToInt8 (boz "'80000000'x") `shouldBe` Int 2147483648
    bozToInt8 (boz "'ffffffff'x") `shouldBe` Int 4294967295
    bozToInt8 (boz "'7fffffffffffffff'x") `shouldBe` Int 9223372036854775807
    bozToInt8 (boz "'8000000000000000'x") `shouldBe` Int (-9223372036854775808)
    bozToInt8 (boz "'ffffffffffffffff'x") `shouldBe` Int (-1)
