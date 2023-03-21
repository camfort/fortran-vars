module SymbolTableSpec where

import           Test.Hspec
import           Util ( des1 )

import           Control.Exception              ( evaluate )
import           Language.Fortran.Extras.Analysis
                                                ( versionedExpandedProgramAnalysis
                                                )
import           Language.Fortran.Util.Files    ( flexReadFile )
import           Language.Fortran.Extras.ProgramFile
                                                ( versionedProgramFile )
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.Map                       as M
import           Data.List.NonEmpty             ( NonEmpty( (:|) ) )
import qualified Data.List.NonEmpty             as NonEmpty
import           Language.Fortran.AST           ( ProgramUnitName(..) )
import           Language.Fortran.Version       ( FortranVersion(..) )
import           Language.Fortran.Analysis      ( initAnalysis )

import           Language.Fortran.Vars          ( programFileModel )
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , Type
                                                , SemType(..)
                                                , CharacterLen(..)
                                                , ExpVal(..)
                                                , SymbolTable
                                                , Dim(..), Dims(..)
                                                )

getSymTable :: String -> ByteString -> String -> SymbolTable
getSymTable p c n =
  let pf  = initAnalysis $ versionedProgramFile Fortran77Legacy p c
      pfm = programFileModel pf
  in  case M.lookup (Named n) pfm of
        Just (st, _) -> st
        Nothing      -> error $ "Failed to lookup unit: " ++ n

getSymTableIO :: String -> ByteString -> String -> IO SymbolTable
getSymTableIO p c n = do
  let incls = ["test"]
  pa <- versionedExpandedProgramAnalysis Fortran77Legacy incls p c
  let pfm = programFileModel pa
  case M.lookup (Named n) pfm of
    Just (st, _) -> return st
    Nothing      -> error $ "Failed to lookup unit: " ++ n

valueOf :: String -> SymbolTable -> ExpVal
valueOf name symTable =
  let Just entry = M.lookup name symTable
  in  case entry of
        SParameter _ pv -> pv
        _               -> error (name ++ " is not a parameter.")

typeOf :: String -> SymbolTable -> Type
typeOf name symTable =
  let Just entry = M.lookup name symTable
  in  case entry of
        SParameter ty _ -> ty
        SVariable  ty _ -> ty
        SDummy ty       -> ty
        _               -> error (name ++ " is not an Entry that has type")

dimensionOf :: String -> SymbolTable -> Maybe [Dim Int]
dimensionOf name symTable =
  let Just entry = M.lookup name symTable
  in  case entry of
        SVariable (TArray _ (DimsExplicitShape dims)) _ -> Just $ NonEmpty.toList dims
        SDummy (TArray _ (DimsExplicitShape dims)) -> Just $ NonEmpty.toList dims
        _ -> error (name ++ " is not an Entry that has static dimension")

dummyOf :: String -> M.Map String SymbolTableEntry -> String
dummyOf name symTable =
  let Just entry = M.lookup name symTable
  in  case entry of
        SDummy (TArray (TCharacter CharLenStar _) _) ->
          "DummyArrayDynamicCharacter"
        SDummy (TCharacter CharLenStar _) -> "DummyDynamicCharacter"
        SDummy (TArray _ DimsExplicitShape{}) -> "DummyStaticArray"
        SDummy (TArray _ _) -> "DummyDynamicArray"
        SDummy _ -> "DummyStaticScalar"
        v -> error (name ++ " is not a DummyVariableEntry it is a " ++ show v)

isDummy :: String -> SymbolTable -> Bool
isDummy name symTable = case M.lookup name symTable of
  Just SDummy{} -> True
  _             -> False

isDynamic :: String -> SymbolTable -> Bool
isDynamic name symTable = case M.lookup name symTable of
  Just (SVariable ty _) -> case ty of
    TArray (TCharacter CharLenStar _) _ -> True
    TArray _ DimsExplicitShape{} -> False
    TArray _ _ -> True
    TCharacter CharLenStar _ -> True
    _ -> False
  _ -> False

spec :: Spec
spec = do

  describe "Constant expression evaluation: " $ do

    let path     = "test/symbol_table/constant_expression.f"
        unitName = "foo"

    it "Only parameter statement, no declaration" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "a" symTable `shouldBe` Int 10

    it "declaration after parameter statement" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "b" symTable `shouldBe` Int 10

    it "declaration before parameter statement" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "r" symTable `shouldBe` Real 3.14

    it "Integer arithmetics with variable" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      mapM_
        (\(var, val) -> valueOf var symTable `shouldBe` Int val)
        [("d1", 12), ("d2", 8), ("d3", 20), ("d4", 3), ("d5", -10), ("d6", 10)]

    it "Float arithmetics with variable" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      mapM_
        (\(var, val) -> valueOf var symTable `shouldBe` Real val)
        [ ("pi", 3.14)
        , ("e1", 6.28)
        , ("e2", 0.0)
        , ("e3", 6.28)
        , ("e4", 1.57)
        , ("e5", -3.14)
        , ("e6", 3.14)
        ]

    it "Double precision constant" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "eps" symTable `shouldBe` Real 1.0e-12

    it "Logical expression" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "f" symTable `shouldBe` Logical True
      valueOf "f1" symTable `shouldBe` Logical False

    it "String expression" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "rcsid" symTable `shouldBe` Str "DEADBEEF"

    it "Intrinsics" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "d7" symTable `shouldBe` Int 9
      valueOf "d8" symTable `shouldBe` Int 60
      valueOf "d9" symTable `shouldBe` Int 70
      valueOf "e7" symTable `shouldBe` Real 1.2
      -- TODO: below is disallowed by gfortran and spec (F2018 16.9.125)
      -- parameter (e8 = max(1.2,2))
      --valueOf "e8" symTable `shouldBe` Real 2
      valueOf "c1" symTable `shouldBe` Str "A"
      valueOf "eol" symTable `shouldBe` Str "\r\n"
      valueOf "i1" symTable `shouldBe` Int (-2)
      valueOf "i2" symTable `shouldBe` Int 42
      valueOf "i3" symTable `shouldBe` Int (-42)
      valueOf "i4" symTable `shouldBe` Int 0
      valueOf "i5" symTable `shouldBe` Int 1
      valueOf "i6" symTable `shouldBe` Int 1
      valueOf "i7" symTable `shouldBe` Int (-1)
      valueOf "i8" symTable `shouldBe` Int (-1)
      valueOf "i9" symTable `shouldBe` Int 16


  describe "Type and Kind: " $ do

    let path     = "test/symbol_table/type_and_kind.f"
        unitName = "foo"

    it "Byte" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "b" symTable `shouldBe` TByte 1

    it "Character" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "c" symTable `shouldBe` TCharacter (CharLenInt 1) 1
      typeOf "c1" symTable `shouldBe` TCharacter (CharLenInt 7) 1
      typeOf "c2" symTable `shouldBe` TCharacter (CharLenInt 7) 1
      typeOf "c3" symTable `shouldBe` TCharacter (CharLenInt 11) 1
      typeOf "c4" symTable `shouldBe` TCharacter (CharLenInt 23) 1
      valueOf "c4" symTable `shouldBe` Str "Hello right back at you"


    it "Integer" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      mapM_ (\(v, k) -> typeOf v symTable `shouldBe` TInteger k)
            [("i", 4), ("i2", 2), ("i4", 4), ("i8", 8)]

    it "Logical" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      mapM_ (\(v, k) -> typeOf v symTable `shouldBe` TLogical k)
            [("l", 4), ("l1", 1), ("l2", 2), ("l4", 4), ("l8", 8)]

    it "Real and Double Precision" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "r" symTable `shouldBe` TReal 4
      typeOf "r4" symTable `shouldBe` TReal 4
      typeOf "r8" symTable `shouldBe` TReal 8

    it "Double Precision" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "dp" symTable `shouldBe` TReal 8

    it "Complex" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "comp" symTable `shouldBe` TComplex 8
      typeOf "comp8" symTable `shouldBe` TComplex 8
      typeOf "comp16" symTable `shouldBe` TComplex 16

    it "Double Complex" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "dcomp" symTable `shouldBe` TComplex 16

--    TODO: Uncomment this test when implementing nonstandard kind handling
    it "Nonstandard kind" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "i2_ns" symTable `shouldBe` TInteger 2
      typeOf "i4_ns" symTable `shouldBe` TInteger 4
      typeOf "i8_ns" symTable `shouldBe` TInteger 8
      typeOf "r2_ns" symTable `shouldBe` TReal 2
      typeOf "r8_ns" symTable `shouldBe` TReal 8
      typeOf "l2_ns" symTable `shouldBe` TLogical 2
      typeOf "l8_ns" symTable `shouldBe` TLogical 8

    it "Nonstandard kind array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "i2_arr" symTable
        `shouldBe` TArray (TInteger 2) (DimsExplicitShape (Dim 1 3 :| [Dim 1 4]))
      typeOf "i8_arr" symTable
        `shouldBe` TArray (TInteger 8) (DimsExplicitShape (Dim 1 3 :| [Dim 1 4]))

  describe "Dimension: " $ do

    let path     = "test/symbol_table/dimension.f"
        unitName = "foo"

    it "Single dimension" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      dimensionOf "a" symTable `shouldBe` Just [Dim 1 10]
      dimensionOf "b" symTable `shouldBe` Just [Dim (-3) 5]
      dimensionOf "c" symTable `shouldBe` Just [Dim 1 45]

    it "Multi-dimension" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      dimensionOf "a2" symTable `shouldBe` Just [Dim 1 5, Dim 1 5]
      dimensionOf "a3" symTable `shouldBe` Just [Dim 1 5, Dim 1 5, Dim 1 5]
      dimensionOf "a4" symTable `shouldBe` Just [Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5]
      dimensionOf "a5" symTable `shouldBe` Just [Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5]
      dimensionOf "a6" symTable `shouldBe` Just [Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5]
      dimensionOf "a7" symTable `shouldBe` Just [Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5, Dim 1 5]

    it "Dimension statement" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      dimensionOf "d" symTable `shouldBe` Just [Dim 1 10]
      dimensionOf "m" symTable `shouldBe` Just [Dim 1 10, Dim 1 20]

    it "String array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      dimensionOf "reqname" symTable `shouldBe` Just [Dim 1 64]
      dimensionOf "test" symTable `shouldBe` Just [Dim 1 3, Dim 1 4]

    it "Integer array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      dimensionOf "itest1" symTable `shouldBe` Just [Dim 1 3, Dim 1 4]
      dimensionOf "itest2" symTable `shouldBe` Just [Dim 1 3, Dim 1 4]


    it "Dimension declaration within COMMON - as ExpSubscript" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dimensionOf "arr_before_range" symTable `shouldBe` Just [Dim 8 10]
      dimensionOf "arr_before_multi" symTable
        `shouldBe` Just [Dim 1 12, Dim 14 16]

      dimensionOf "arr_after_range" symTable `shouldBe` Just [Dim 22 24]
      dimensionOf "arr_after_multi" symTable `shouldBe` Just [Dim 1 26, Dim 28 30]

    it "Dimension declaration within COMMON - as ExpFunctionCall" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dimensionOf "arr_before_standard_kind" symTable `shouldBe` Just [Dim 1 2]
      dimensionOf "arr_before_simple" symTable `shouldBe` Just [Dim 1 4]
      dimensionOf "arr_before_nonstandard_kind" symTable
        `shouldBe` Just [Dim 1 6]

      dimensionOf "arr_after_standard_kind" symTable `shouldBe` Just [Dim 1 18]
      dimensionOf "arr_after_simple" symTable `shouldBe` Just [Dim 1 20]
      dimensionOf "arr_after_nonstandard_kind" symTable `shouldBe` Just [Dim 1 2]

  describe "Dummy Argument: " $ do

    let path     = "test/symbol_table/dummy_argument_symbol.f"
        unitName = "sub"

    it "Dummy variables - static" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dummyOf "stscalar1" symTable `shouldBe` "DummyStaticScalar"
      typeOf "stscalar1" symTable `shouldBe` TInteger 4

      dummyOf "starr1" symTable `shouldBe` "DummyStaticArray"
      typeOf "starr1" symTable `shouldBe` TArray (TInteger 4) (des1 1 5)

    it "Dummy variables - dynamic" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dummyOf "dynscalar1" symTable `shouldBe` "DummyDynamicCharacter"

      dummyOf "dynarr1" symTable `shouldBe` "DummyDynamicArray"
      typeOf "dynarr1" symTable `shouldBe` TArray (TInteger 4) (DimsAssumedSize Nothing 1)

      dummyOf "dynarr2" symTable `shouldBe` "DummyDynamicArray"
      typeOf "dynarr2" symTable `shouldBe` TArray (TInteger 4) undefined

      dummyOf "dynarr3" symTable `shouldBe` "DummyDynamicArray"
      typeOf "dynarr3" symTable `shouldBe` TArray (TInteger 4) (DimsAssumedSize (Just (Dim 1 3 :| [])) 1)

      dummyOf "dynarr4" symTable `shouldBe` "DummyDynamicArray"
      typeOf "dynarr4" symTable `shouldBe` TArray (TInteger 4) undefined

      evaluate (dummyOf "dynarr5" symTable) `shouldThrow` anyErrorCall
      typeOf "dynarr5" symTable `shouldBe` TArray (TInteger 4) undefined

  describe "Dummy array of dynamically-sized strings" $ do
    let path     = "test/symbol_table/dummy_array_dynamic_strings.f"
        unitName = "sub"

    it "Statically-sized dummy array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dummyOf "starrdynstring" symTable `shouldBe` "DummyArrayDynamicCharacter"

    it "Dynamically-sized dummy array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName

      dummyOf "dynarrdynstring" symTable `shouldBe` "DummyArrayDynamicCharacter"

  describe "Include: " $ do

    let path           = "test/symbol_table/include.f"
        path_interface = "test/symbol_table/interface-include.f"
        unitName       = "main"

    it "Simple include" $ do
      contents <- flexReadFile path
      symTable <- getSymTableIO path contents unitName

      valueOf "var_from_includee" symTable `shouldBe` Str "includee"

    it "Interface include" $ do
      -- TODO 2021-09-06: fails with fortran-src ParseError: interface.inc: lexing failed
      pending
      contents <- flexReadFile path_interface
      symTable <- getSymTableIO path_interface contents unitName

      typeOf "sespit_get_psetdt" symTable
        `shouldBe` TArray (TInteger 2) (des1 1 3)
      typeOf "sespit_get_psetdt2_e" symTable `shouldBe` TInteger 2
      -- Check we don't pick up subroutines or arguments
      M.member "index" symTable `shouldBe` False
      M.member "sespit_set_trdnum" symTable `shouldBe` False

  describe "BOZ constants: " $ do

    let path_general = "test/boz_constants/boz_general.f"
        path_integer = "test/boz_constants/boz_integer.f"
        unitName     = "main"

    it "Suffix notation" $ do
      contents <- flexReadFile path_general
      let symTable = getSymTable path_general contents unitName

      valueOf "boz_suffix_binary" symTable `shouldBe` Int 1
      valueOf "boz_suffix_octal" symTable `shouldBe` Int 1
      valueOf "boz_suffix_hex_x" symTable `shouldBe` Int 1
      valueOf "boz_suffix_hex_z" symTable `shouldBe` Int 1

    it "Prefix notation" $ do
      contents <- flexReadFile path_general
      let symTable = getSymTable path_general contents unitName

      valueOf "boz_prefix_binary" symTable `shouldBe` Int 1
      valueOf "boz_prefix_octal" symTable `shouldBe` Int 1
      valueOf "boz_prefix_hex_x" symTable `shouldBe` Int 1
      valueOf "boz_prefix_hex_z" symTable `shouldBe` Int 1

    it "Supported types" $ do
      contents <- flexReadFile path_general
      let symTable = getSymTable path_general contents unitName

      valueOf "boz_integer" symTable `shouldBe` Int 1

    it "Type Integer" $ do
      contents <- flexReadFile path_integer
      let symTable = getSymTable path_integer contents unitName

      valueOf "i2_1" symTable `shouldBe` Int 1
      valueOf "i2_2" symTable `shouldBe` Int 16
      valueOf "i2_3" symTable `shouldBe` Int 256
      valueOf "i2_4" symTable `shouldBe` Int 4096
      valueOf "i2_5" symTable `shouldBe` Int 32767
      valueOf "i2_6" symTable `shouldBe` Int (-32768)
      valueOf "i2_7" symTable `shouldBe` Int (-32767)
      valueOf "i2_8" symTable `shouldBe` Int 0
      valueOf "i2_9" symTable `shouldBe` Int 16
      valueOf "i2_10" symTable `shouldBe` Int 16

  describe "Floating point types" $ do
    it "Can parse all floating point types" $ do
      let path     = "test/symbol_table/floating_points.f"
          unitName = "floating_points"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      valueOf "a" symTable `shouldBe` Real 1.0
      valueOf "b" symTable `shouldBe` Real 2.0
      valueOf "c" symTable `shouldBe` Real 3.0
      valueOf "d" symTable `shouldBe` Real 4.0
      valueOf "e" symTable `shouldBe` Real 0.5
      valueOf "f" symTable `shouldBe` Real 6.0
      valueOf "g" symTable `shouldBe` Real 0.7

    it "Doesn't accept bad formats" $ do
      let path     = "test/symbol_table/bad_floating_point.f"
          unitName = "bad_floating_point"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      print (valueOf "a" symTable) `shouldThrow` anyException

  describe "Parameter statement" $ do
    let errStr t = "Invalid PARAMETER statement for symbol \'" ++ t ++ "\'"
    it "Throws on conflicting PARAMETER attribute" $ do
      let path     = "test/symbol_table/multi_parameter_defs.f"
          unitName = "multi_parameter_defs"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      print (typeOf "a" symTable) `shouldThrow` errorCall (errStr "a")

    it "Can handle declarations and parameter statements" $ do
      let path     = "test/symbol_table/parameter_stmt.f"
          unitName = "parameter_stmt"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "c1" symTable `shouldBe` TCharacter (CharLenInt 7) 1
      typeOf "c2" symTable `shouldBe` TCharacter (CharLenInt 11) 1

      typeOf "i" symTable `shouldBe` TInteger 4
      typeOf "i2" symTable `shouldBe` TInteger 2
      typeOf "i4" symTable `shouldBe` TInteger 4
      typeOf "i8" symTable `shouldBe` TInteger 8

      typeOf "r" symTable `shouldBe` TReal 4
      typeOf "r2" symTable `shouldBe` TReal 2
      typeOf "r4" symTable `shouldBe` TReal 4
      typeOf "r8" symTable `shouldBe` TReal 8

  describe "Multiple declarations" $ do
    it "Can handle multiple declarations of the same type and kind" $ do
      let path     = "test/symbol_table/multi_declarations.f"
          unitName = "barfoo"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "foobar" symTable `shouldBe` TLogical 4
      typeOf "str" symTable `shouldBe` TCharacter (CharLenInt 5) 1

    let
      errStr t =
        "The second declaration of 'a' at line (3:19)-(3:19) does not have the same "
          ++ t
          ++ " as the first"
    it "Throws on conflicting types" $ do
      let path     = "test/symbol_table/conflicting_type_defs.f"
          unitName = "conflicting_type_defs"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      print (typeOf "a" symTable) `shouldThrow` errorCall (errStr "type")
    it "Overriding kinds" $ do
      let path     = "test/symbol_table/overriding_kind_defs.f"
          unitName = "overriding_kind_defs"
      contents <- flexReadFile path
      let symTable = getSymTable path contents unitName
      typeOf "a" symTable `shouldBe` TInteger 4
      typeOf "b" symTable `shouldBe` TCharacter (CharLenInt 10) 1
      typeOf "c" symTable `shouldBe` TInteger 4
      typeOf "d" symTable `shouldBe` TCharacter (CharLenInt 10) 1
      typeOf "e" symTable `shouldBe` TCharacter (CharLenInt 5) 1

  describe "Function variables" $ do
    let path = "test/symbol_table/function_variable.f"
    it "Can handle variable from siganture-typed function (default kind)" $ do
      contents <- flexReadFile path
      let unitName = "f1"
          symTable = getSymTable path contents unitName
      typeOf "f1" symTable `shouldBe` TLogical 4
    it "Can handle variable from siganture-typed function (specified kind)" $ do
      contents <- flexReadFile path
      let unitName = "f2"
          symTable = getSymTable path contents unitName
      typeOf "f2" symTable `shouldBe` TLogical 2
    it "Can handle variable from siganture-typed function (character type)" $ do
      contents <- flexReadFile path
      let unitName = "f3"
          symTable = getSymTable path contents unitName
      typeOf "f3" symTable `shouldBe` TCharacter (CharLenInt 5) 1
    it "Can handle variable from body-typed function" $ do
      contents <- flexReadFile path
      let unitName = "f4"
          symTable = getSymTable path contents unitName
      typeOf "f4" symTable `shouldBe` TLogical 4
    it "Can handle duplicate declarations in signature and function body" $ do
      contents <- flexReadFile path
      let unitName = "f5"
          symTable = getSymTable path contents unitName
      typeOf "f5" symTable `shouldBe` TInteger 4
    it
        "Can handle duplicate declarations in signature and function body - String"
      $ do
          contents <- flexReadFile path
          let unitName = "f6"
              symTable = getSymTable path contents unitName
          typeOf "f6" symTable `shouldBe` TCharacter (CharLenInt 6) 1
    it "Can handle dynamic character function signature" $ do
      contents <- flexReadFile path
      let unitName = "f7"
          symTable = getSymTable path contents unitName
      typeOf "f7" symTable `shouldBe` TCharacter CharLenStar 1

  describe "Dynamic variables" $ do
    let path = "test/symbol_table/dynamic_variables.f"

    it "Dynamic character" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents "f1"
      typeOf "c" symTable `shouldBe` TCharacter CharLenStar 1
      isDynamic "c" symTable `shouldBe` True

    it "Dynamic array" $ do
      contents <- flexReadFile path
      let symTable = getSymTable path contents "f2"
      typeOf "arr" symTable `shouldBe` TArray (TInteger 4) undefined
      isDynamic "arr" symTable `shouldBe` True

    it "Dynamic character Dynamic array" $ do
      contents <- flexReadFile path
      let st = getSymTable path contents "f3"
      typeOf "arr" st `shouldBe` TArray (TCharacter CharLenStar 1) undefined
      isDynamic "arr" st `shouldBe` True

    it "Dynamic character static array" $ do
      contents <- flexReadFile path
      let st = getSymTable path contents "f4"
      typeOf "arr" st
        `shouldBe` TArray (TCharacter CharLenStar 1) (des1 1 5)
      isDynamic "arr" st `shouldBe` True

    it "Static character dynamic array" $ do
      contents <- flexReadFile path
      let st = getSymTable path contents "f5"
      typeOf "arr" st `shouldBe` TArray (TCharacter (CharLenInt 5) 1) undefined
      isDynamic "arr" st `shouldBe` True

    it "Dummy not dynamic" $ do
      contents <- flexReadFile path
      let st = getSymTable path contents "f6"
      typeOf "arr" st `shouldBe` TArray (TCharacter CharLenStar 1) undefined
      isDynamic "arr" st `shouldBe` False
      isDummy "arr" st `shouldBe` True
