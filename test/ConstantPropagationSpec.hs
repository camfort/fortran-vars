module ConstantPropagationSpec where

import           Test.Hspec

import           Language.Fortran.Extras
                                                ( allS )
import           Language.Fortran.Extras.Test
                                                ( getTestProgramAnalysis )
import           Language.Fortran.AST
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )

import           Language.Fortran.Vars.Types
import           Language.Fortran.Vars.ConstantPropagation
                                                ( constantPropagationValue
                                                , ValueOf
                                                )
import           Language.Fortran.Vars.CPValue
                                                ( CPValue(..) )
import           Language.Fortran.Vars.Call
                                                ( functionCalls
                                                , subroutineCalls
                                                , functionArguments
                                                , subroutineArguments
                                                )

-- | getRHSFunc provide a check point in the program file to test
-- the value of an expression.
-- It grab the right-hand side expression from an assignment statement.
-- For code like `lhsName = rhs`
-- `getRHSFunc pf "lhsName"` will return expression `rhs`
-- The lhsName shall be unique in the Program File
getRHSFunc :: ProgramFile (Analysis A0) -> Name -> Expression (Analysis A0)
getRHSFunc pf lhs = head
  [ rhs
  | (StExpressionAssign _ _ e@(ExpValue _ _ ValVariable{}) rhs) <- allS pf
  , srcName e == lhs
  ]

helper :: String -> IO (ValueOf A0, Name -> Expression (Analysis A0))
helper path = do
  pf <- getTestProgramAnalysis path
  let valueOf = constantPropagationValue pf
      getRHS  = getRHSFunc pf
  return (valueOf, getRHS)

spec :: Spec
spec = do

  describe "Variable" $ do

    let path = "test/constant_propagation/variable.f"
    it "Branch" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "checka") `shouldBe` Const (Int 1)
      valueOf (getRHS "checkc") `shouldBe` Top
      valueOf (getRHS "checkb") `shouldBe` Bot

    it "Equivalence" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "checkd") `shouldBe` Const (Int 123)


  describe "Short Circuit" $ do
    let path = "test/constant_propagation/short_circuit.f"
    it "Logical Operation" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "logical1") `shouldBe` Const (Logical False)
      valueOf (getRHS "logical2") `shouldBe` Const (Logical False)
      valueOf (getRHS "logical3") `shouldBe` Const (Logical True)
      valueOf (getRHS "logical4") `shouldBe` Const (Logical True)

    it "Multiplication" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "int1") `shouldBe` Const (Int 0)
      valueOf (getRHS "int2") `shouldBe` Const (Int 0)
      valueOf (getRHS "real1") `shouldBe` Const (Real 0.0)
      valueOf (getRHS "real2") `shouldBe` Const (Real 0.0)

    it "Real exponentiation" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "real3") `shouldBe` Const (Real 1.0)
      valueOf (getRHS "real4") `shouldBe` Const (Real 1.0)
      valueOf (getRHS "real5") `shouldBe` Const (Real 1.0)
      valueOf (getRHS "real6") `shouldBe` Const (Real 1.0)

    it "Integer exponentiation" $ do
      (valueOf, getRHS) <- helper "test/constant_propagation/integer_exponent.f"
      valueOf (getRHS "a") `shouldBe` Const (Int 2)
      valueOf (getRHS "b") `shouldBe` Const (Int 2)
      valueOf (getRHS "c") `shouldBe` Const (Int 4)

  describe "Strings" $ do

    it "string scalar" $ do
      (valueOf, getRHS) <- helper "test/constant_propagation/string_scalar.f"

      valueOf (getRHS "s1") `shouldBe` Const (Str "A")
      valueOf (getRHS "s4") `shouldBe` Const (Str "4")

    it "string array" $ do
      (valueOf, getRHS) <- helper "test/constant_propagation/string_array.f"

      valueOf (getRHS "a1") `shouldBe` Const (Str "A")
      valueOf (getRHS "a4") `shouldBe` Const (Str "A")

  describe "Array" $ do
    let path = "test/constant_propagation/array.f"

    it "Branch" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a1") `shouldBe` Const (Int 1)
      valueOf (getRHS "a2") `shouldBe` Bot
      valueOf (getRHS "a3") `shouldBe` Top

    it "Equivalence" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a4") `shouldBe` Const (Int 123)

    it "Unknown index on RHS" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a5") `shouldBe` Bot

    it "Unknown index on LHS" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a6") `shouldBe` Bot
      valueOf (getRHS "a7") `shouldBe` Bot
      valueOf (getRHS "a8") `shouldBe` Bot
      valueOf (getRHS "a9") `shouldBe` Bot

    it "Update array after processing unknown index on LHS" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a10") `shouldBe` Const (Int 456)
      valueOf (getRHS "a11") `shouldBe` Bot


  describe "Loop" $ do
    let path = "test/constant_propagation/loop.f"

    it "Loop" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a1") `shouldBe` Bot
      valueOf (getRHS "a2") `shouldBe` Const (Int 123)
      valueOf (getRHS "a3") `shouldBe` Const (Int 456)

    it "Do Loop" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "b1") `shouldBe` Bot
      valueOf (getRHS "b2") `shouldBe` Bot
      valueOf (getRHS "b3") `shouldBe` Const (Int 123)
      valueOf (getRHS "b4") `shouldBe` Const (Int 456)


  describe "Arithmetic If" $ do
    let path = "test/constant_propagation/arithif.f"

    it "Arithmetic If" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a1") `shouldBe` Top
      valueOf (getRHS "a2") `shouldBe` Bot
      valueOf (getRHS "a3") `shouldBe` Const (Int 123)

  describe "Subroutine" $ do
    let path = "test/constant_propagation/subroutine.f"

    it "Calling unary subroutine" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a1") `shouldBe` Const (Int 1)
      valueOf (getRHS "a2") `shouldBe` Const (Int 1)
      valueOf (getRHS "a3") `shouldBe` Bot
      valueOf (getRHS "a4") `shouldBe` Bot

    it "Calling binary subroutine" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "b1") `shouldBe` Const (Int 1)
      valueOf (getRHS "b2") `shouldBe` Const (Int 2)
      valueOf (getRHS "b3") `shouldBe` Bot
      valueOf (getRHS "b4") `shouldBe` Bot

    it "Calling array subroutine" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "c1") `shouldBe` Const (Int 1)
      valueOf (getRHS "c2") `shouldBe` Const (Int 2)
      valueOf (getRHS "c3") `shouldBe` Bot
      valueOf (getRHS "c4") `shouldBe` Bot

  describe "Function" $ do
    let path = "test/constant_propagation/function.f"
    it "Calling unary function" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "a1") `shouldBe` Const (Int 2)
      valueOf (getRHS "a2") `shouldBe` Const (Int 2)
      valueOf (getRHS "a3") `shouldBe` Bot
      valueOf (getRHS "a4") `shouldBe` Bot
      valueOf (getRHS "a5") `shouldBe` Bot

    it "Calling binary function" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "b1") `shouldBe` Const (Int 3)
      valueOf (getRHS "b2") `shouldBe` Const (Int 4)
      valueOf (getRHS "b3") `shouldBe` Bot
      valueOf (getRHS "b4") `shouldBe` Bot
      valueOf (getRHS "b5") `shouldBe` Bot

    it "Calling array subroutine" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "e1") `shouldBe` Const (Int 1)
      valueOf (getRHS "e2") `shouldBe` Const (Int 2)
      valueOf (getRHS "e3") `shouldBe` Bot
      valueOf (getRHS "e4") `shouldBe` Bot
      valueOf (getRHS "e5") `shouldBe` Bot

    it "Dummy in unary function" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "c1") `shouldBe` Bot
      valueOf (getRHS "c2") `shouldBe` Bot

    it "Dummies in binary function" $ do
      (valueOf, getRHS) <- helper path

      valueOf (getRHS "d1") `shouldBe` Bot
      valueOf (getRHS "d2") `shouldBe` Bot
      valueOf (getRHS "d3") `shouldBe` Bot

  describe "Check Function Arguments" $ do
    let path = "test/constant_propagation/function_arguments.f"
    it "Function Arguments" $ do
      pf <- getTestProgramAnalysis path
      let valueOf = constantPropagationValue pf
          bars    = functionCalls pf "bar"
          bar1    = head bars
          bar2    = bars !! 1
          bar3    = bars !! 2
      map valueOf (functionArguments bar1)
        `shouldBe` [Const (Int 1), Const (Int 2)]
      map valueOf (functionArguments bar2) `shouldBe` [Bot, Bot]
      map valueOf (functionArguments bar3) `shouldBe` [Const (Int 3), Bot]

  describe "Check Subroutine Arguments" $ do
    let path = "test/constant_propagation/subroutine_arguments.f"
    it "Subroutine Arguments" $ do
      pf <- getTestProgramAnalysis path
      let valueOf = constantPropagationValue pf
          bars    = subroutineCalls pf "bar"
          call1   = head bars
          call2   = bars !! 1
          call3   = bars !! 2
          call4   = bars !! 3
      map valueOf (subroutineArguments call1)
        `shouldBe` [Const (Int 1), Const (Int 2)]
      map valueOf (subroutineArguments call2) `shouldBe` [Bot, Bot]
      map valueOf (subroutineArguments call3)
        `shouldBe` [Const (Int 3), Const (Int 7)]
      map valueOf (subroutineArguments call4)
        `shouldBe` [Const (Int 0), Const (Int 1)]

  describe "Commmon Variables" $ do
    let path = "test/constant_propagation/common.f"
    it "Skip common variable" $ do
      (valueOf, getRHS) <- helper path
      valueOf (getRHS "before1") `shouldBe` Top
      valueOf (getRHS "before2") `shouldBe` Top
      valueOf (getRHS "after1") `shouldBe` Top
      valueOf (getRHS "after2") `shouldBe` Top

  describe "Arrays as Parameters" $ do
    let path = "test/constant_propagation/array_params.f"
    it "Marks arrays passed as parameters as Bot" $ do
      (valueOf, getRHS) <- helper path
      valueOf (getRHS "b") `shouldBe` Bot
      valueOf (getRHS "c") `shouldBe` Bot

  describe "Lattice operation of CPValue" $ do
    let path = "test/constant_propagation/cpvalue.f"
    it "cpvalue" $ do
      (valueOf, getRHS) <- helper path
      valueOf (getRHS "c1") `shouldBe` Top
      valueOf (getRHS "c2") `shouldBe` Top
      valueOf (getRHS "c3") `shouldBe` Top
      valueOf (getRHS "c4") `shouldBe` Bot
      valueOf (getRHS "c5") `shouldBe` Bot
      valueOf (getRHS "c6") `shouldBe` Const (Int 3)
      valueOf (getRHS "c7") `shouldBe` Bot
      valueOf (getRHS "c8") `shouldBe` Bot
      valueOf (getRHS "c9") `shouldBe` Bot

  describe "Multiple ExpSubscripts" $ do
    let path = "test/constant_propagation/multi_subscript.f"
    it "Can handle multiple chained subscripts" $ do
      (valueOf, getRHS) <- helper path
      valueOf (getRHS "p") `shouldBe` Top
      valueOf (getRHS "q") `shouldBe` Top

  describe "Subscripts" $ do
    it "Can correctly determine a simple substring" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/simple_subscripts.f"
      valueOf (getRHS "a") `shouldBe` Const (Str "0123456789")
      valueOf (getRHS "b") `shouldBe` Const (Str "01234")

    it "Can correctly determine values of strings after subscripting" $ do
      (valueOf, getRHS) <- helper "test/constant_propagation/subscripts.f"
      valueOf (getRHS "bottom") `shouldBe` Bot
      valueOf (getRHS "top") `shouldBe` Top
      valueOf (getRHS "c") `shouldBe` Const (Str "&&&&&&&&&&")
      valueOf (getRHS "d") `shouldBe` Const (Str "0123456789")
      valueOf (getRHS "const1") `shouldBe` Const (Str "0123456789")
      valueOf (getRHS "const2") `shouldBe` Const (Str "01234")
      valueOf (getRHS "const3") `shouldBe` Const (Str "56789")

  describe "Array Truncation" $ do
    it "Can handle integer array truncation" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/array_truncation_integer.f"
      valueOf (getRHS "a1") `shouldBe` Const (Int 8)
      valueOf (getRHS "a2") `shouldBe` Const (Int 8)
      valueOf (getRHS "a3") `shouldBe` Const (Int 8)
      valueOf (getRHS "a4") `shouldBe` Const (Int 8)

    it "Can handle string array truncation" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/array_truncation_string.f"
      valueOf (getRHS "a1") `shouldBe` Const (Str "A")
      valueOf (getRHS "a2") `shouldBe` Const (Str "A")
      valueOf (getRHS "a3") `shouldBe` Const (Str "A")
      valueOf (getRHS "a4") `shouldBe` Const (Str "A")

  describe "Null strings"
    $ it "Should treat empty string as a space in assignments and comparisons"
    $ do
        (valueOf, getRHS) <- helper "test/constant_propagation/null_string.f"
        valueOf (getRHS "b") `shouldBe` Const (Str "12 45")
        valueOf (getRHS "c") `shouldBe` Const (Str "A")
        valueOf (getRHS "d") `shouldBe` Const (Str "B")

  describe "Array sections" $ do
    let errStr = "Array sections are not allowed in FORTRAN 77"

    it "Should not allow assignment to string array sections" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/string_array_sections.f"
      print (valueOf $ getRHS "b") `shouldThrow` errorCall errStr

    it "Should not allow assignment to integer array sections" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/integer_array_sections.f"
      print (valueOf $ getRHS "b") `shouldThrow` errorCall errStr

    it "Should not allow assignment from string array sections" $ do
      (valueOf, getRHS) <- helper
        "test/constant_propagation/array_section_assignment.f"
      print (valueOf $ getRHS "b") `shouldThrow` errorCall errStr

  describe "Expressions in substrings"
    $ it "Should correctly evaluate various substrings indexed with expressions"
    $ do
        (valueOf, getRHS) <- helper
          "test/constant_propagation/substring_index_expressions.f"
        valueOf (getRHS "a") `shouldBe` Const (Str "12345")
        valueOf (getRHS "j") `shouldBe` Const (Int 3)
        valueOf (getRHS "b") `shouldBe` Const (Str "45")
        valueOf (getRHS "c") `shouldBe` Const (Str "4")
        valueOf (getRHS "d") `shouldBe` Bot
        valueOf (getRHS "e") `shouldBe` Bot
