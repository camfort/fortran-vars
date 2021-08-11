module BozConstantSpec where

import           Test.Hspec

import           Language.Fortran.Vars.BozConstant
                                                ( bozToInt2
                                                , bozToInt4
                                                , bozToInt8
                                                )
import           Language.Fortran.Vars.Types
                                                ( ExpVal(..) )


spec :: Spec
spec = describe "Boz Constant Conversion" $ do
  it "Mixed case BOZ constant conversions" $ do
    bozToInt2 (Boz "'ffff'x") `shouldBe` Int (-1)
    bozToInt2 (Boz "'FfFf'x") `shouldBe` Int (-1)
    bozToInt4 (Boz "'FFFFFFFF'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*2" $ do
    bozToInt2 (Boz "'1'x") `shouldBe` Int 1
    bozToInt2 (Boz "'10'x") `shouldBe` Int 16
    bozToInt2 (Boz "'7fff'x") `shouldBe` Int 32767
    bozToInt2 (Boz "'8000'x") `shouldBe` Int (-32768)
    bozToInt2 (Boz "'ffff'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*4" $ do
    bozToInt4 (Boz "'1'x") `shouldBe` Int 1
    bozToInt4 (Boz "'10'x") `shouldBe` Int 16
    bozToInt4 (Boz "'7fff'x") `shouldBe` Int 32767
    bozToInt4 (Boz "'8000'x") `shouldBe` Int 32768
    bozToInt4 (Boz "'ffff'x") `shouldBe` Int 65535
    bozToInt4 (Boz "'7fffffff'x") `shouldBe` Int 2147483647
    bozToInt4 (Boz "'80000000'x") `shouldBe` Int (-2147483648)
    bozToInt4 (Boz "'ffffffff'x") `shouldBe` Int (-1)

  it "BOZ constant to Integer*8" $ do
    bozToInt8 (Boz "'1'x") `shouldBe` Int 1
    bozToInt8 (Boz "'10'x") `shouldBe` Int 16
    bozToInt8 (Boz "'7fff'x") `shouldBe` Int 32767
    bozToInt8 (Boz "'8000'x") `shouldBe` Int 32768
    bozToInt8 (Boz "'ffff'x") `shouldBe` Int 65535
    bozToInt8 (Boz "'7fffffff'x") `shouldBe` Int 2147483647
    bozToInt8 (Boz "'80000000'x") `shouldBe` Int 2147483648
    bozToInt8 (Boz "'ffffffff'x") `shouldBe` Int 4294967295
    bozToInt8 (Boz "'7fffffffffffffff'x") `shouldBe` Int 9223372036854775807
    --bozToInt8 (Boz "'8000000000000000'x") `shouldBe` Int (-9223372036854775808)
    --bozToInt8 (Boz "'ffffffffffffffff'x") `shouldBe` Int (-1)
