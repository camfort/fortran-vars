-- TODO 2023-05-16 raehik: only tests the deprecated short circuit eval, which
-- doesn't yet have a parallel in fortran-src eval

module EvalSpec where

import           Test.Hspec

import           Prelude                 hiding ( EQ )
import qualified Data.Map                      as M
import           Language.Fortran.AST           ( A0
                                                , BinaryOp(..)
                                                , Expression(..)
                                                , UnaryOp(..)
                                                , Value(..)
                                                )
import           Language.Fortran.Util.Position ( SrcSpan(..)
                                                , Position(..)
                                                )

import qualified Language.Fortran.Vars.Eval.Deprecated as Deprecated
import           Language.Fortran.Vars.Types
                                                ( ExpVal(..)
                                                , SymbolTable
                                                )

dSpan :: SrcSpan
dSpan = let p = Position 0 0 0 "" Nothing in SrcSpan p p

dSym :: SymbolTable
dSym = M.empty

true, false :: Expression A0
true = ExpValue () dSpan $ ValLogical True Nothing
false = ExpValue () dSpan $ ValLogical False Nothing

foobar :: Expression A0
foobar = ExpValue () dSpan $ ValVariable "foobar"

spec :: Spec
spec = describe "Boolean constant folding" $ do
  it "Can handle Not" $ do
    let ex1 = ExpUnary () dSpan Not true
        ex2 = ExpUnary () dSpan Not false
    Deprecated.evalWithShortcircuit dSym ex1 `shouldBe` Right (Logical False)
    Deprecated.evalWithShortcircuit dSym ex2 `shouldBe` Right (Logical True)
  it "Can handle simple Or" $ do
    let ex1 = ExpBinary () dSpan Or true true
        ex2 = ExpBinary () dSpan Or true false
        ex3 = ExpBinary () dSpan Or false true
        ex4 = ExpBinary () dSpan Or false false
    Deprecated.evalWithShortcircuit dSym ex1 `shouldBe` Right (Logical True)
    Deprecated.evalWithShortcircuit dSym ex2 `shouldBe` Right (Logical True)
    Deprecated.evalWithShortcircuit dSym ex3 `shouldBe` Right (Logical True)
    Deprecated.evalWithShortcircuit dSym ex4 `shouldBe` Right (Logical False)
  it "Can handle simple And" $ do
    let ex1 = ExpBinary () dSpan And true true
        ex2 = ExpBinary () dSpan And true false
        ex3 = ExpBinary () dSpan And false true
        ex4 = ExpBinary () dSpan And false false
    Deprecated.evalWithShortcircuit dSym ex1 `shouldBe` Right (Logical True)
    Deprecated.evalWithShortcircuit dSym ex2 `shouldBe` Right (Logical False)
    Deprecated.evalWithShortcircuit dSym ex3 `shouldBe` Right (Logical False)
    Deprecated.evalWithShortcircuit dSym ex4 `shouldBe` Right (Logical False)
  it "Can handle Or with variable" $ do
    let ex1 = ExpBinary () dSpan Or true foobar
        ex2 = ExpBinary () dSpan Or foobar true
    Deprecated.evalWithShortcircuit dSym ex1 `shouldBe` Right (Logical True)
    Deprecated.evalWithShortcircuit dSym ex2 `shouldBe` Right (Logical True)
  it "Can handle And with variable" $ do
    let ex1 = ExpBinary () dSpan And false foobar
        ex2 = ExpBinary () dSpan And foobar false
    Deprecated.evalWithShortcircuit dSym ex1 `shouldBe` Right (Logical False)
    Deprecated.evalWithShortcircuit dSym ex2 `shouldBe` Right (Logical False)
  it "Can handle more complicated trees" $ do
    -- (foobar .AND. .TRUE.) .AND.
    --    (foobar .AND. (.FALSE. .OR. (foobar .AND. .FALSE.)))
    let lhs = ExpBinary () dSpan And foobar true
        ex1 = ExpBinary () dSpan And foobar false
        ex2 = ExpBinary () dSpan Or false ex1
        rhs = ExpBinary () dSpan And foobar ex2
        ex  = ExpBinary () dSpan And lhs rhs
    Deprecated.evalWithShortcircuit dSym ex `shouldBe` Right (Logical False)
  it "Can handle .NOT. (foobar .AND. .FALSE.)" $ do
    let ex1 = ExpBinary () dSpan And foobar false
        ex  = ExpUnary () dSpan Not ex1
    Deprecated.evalWithShortcircuit dSym ex `shouldBe` Right (Logical True)
  it "Can handle conditions with non-logical logic" $ do
    -- .TRUE. .EQ. 1
    let vx = ExpValue () dSpan $ ValInteger "1" Nothing
        ex = ExpBinary () dSpan EQ true vx
    Deprecated.evalWithShortcircuit dSym ex `shouldBe` Right (Logical True)
