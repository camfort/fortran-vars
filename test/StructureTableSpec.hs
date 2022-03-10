module StructureTableSpec where

import qualified Data.Map                      as M
import           Data.Generics.Uniplate.Data

import           Test.Hspec

import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.AST           ( ProgramUnit
                                                , Statement(..)
                                                , Expression(..)
                                                )
import           Language.Fortran.Extras        ( allPU )
import           Language.Fortran.Extras.Test   ( getTestProgramAnalysis )

import           Language.Fortran.Vars.StructureTable
                                                ( StructureTable
                                                , collectStructures
                                                , StructureTableEntry(..)
                                                , hasEntry
                                                )
import           Language.Fortran.Vars.SymbolTable
                                                ( collectSymbols )
import           Language.Fortran.Vars.Types    ( SymbolTable
                                                , SemType(..)
                                                , CharacterLen(..)
                                                )
import           Language.Fortran.Vars.TypeCheck
                                                ( typeOf )

getStructureTable :: String -> IO StructureTable
getStructureTable file =
  testStructureTable file $ \_ structTable _ -> return structTable

testStructureTable
  :: String
  -> (ProgramUnit (Analysis ()) -> StructureTable -> SymbolTable -> IO a)
  -> IO a
testStructureTable fp logic = do
  pf <- getTestProgramAnalysis fp
  let pu = head $ allPU pf
  testStructureTablePU pu logic

testStructureTablePU
  :: ProgramUnit (Analysis ())
  -> (ProgramUnit (Analysis ()) -> StructureTable -> SymbolTable -> IO a)
  -> IO a
testStructureTablePU pu logic = do
  let symbolTable = collectSymbols pu
      structTable = collectStructures symbolTable pu
  logic pu structTable symbolTable

spec :: Spec
spec = do
  describe "Field tests" $ do
    it "test single field" $ do
      structTable <- getStructureTable "test/structure_table/basic_structure.f"
      let expStruct = M.fromList [("foo", [FieldEntry "bar" (TInteger 2)])]
      structTable `shouldBe` expStruct

    it "test structure referenced in structure" $ do
      structTable <- getStructureTable "test/structure_table/structure2.f"
      let expStruct = M.fromList
            [ ("foo", [FieldEntry "bar" (TInteger 8)])
            , ( "baz"
              , [ FieldEntry "qux"  (TCustom "foo")
                , FieldEntry "quux" (TCharacter (CharLenInt 10) 1)
                ]
              )
            ]
      structTable `shouldBe` expStruct

    it "multiple layer structures" $ do
      structTable <- getStructureTable "test/structure_table/structure3.f"
      let graultEntries = [FieldEntry "garply" (TCustom "quuz")]
          quuzEntries =
            [ FieldEntry "corge"  (TCustom "baz")
            , FieldEntry "foobar" (TInteger 2)
            ]
          fooEntries = [FieldEntry "bar" (TInteger 8)]
          bazEntries =
            [ FieldEntry "qux"  (TCustom "foo")
            , FieldEntry "quux" (TCharacter (CharLenInt 10) 1)
            ]
      structTable `shouldBe` M.fromList
        [ ("grault", graultEntries)
        , ("quuz"  , quuzEntries)
        , ("foo"   , fooEntries)
        , ("baz"   , bazEntries)
        ]

    it "structures and subscripts" $ do
      pf <- getTestProgramAnalysis "test/structure_table/structure4.f"
      let pus         = allPU pf
          sts         = map collectSymbols pus
          strctTables = zipWith collectStructures sts pus
      head strctTables `shouldBe` M.fromList
        [ ( "str_inner"
          , [FieldEntry "inner_arr" (TArray (TInteger 1) (Just [(1, 5)]))]
          )
        ]
      (strctTables !! 1) `shouldBe` M.fromList
        [ ( "str_inner"
          , [FieldEntry "inner_arr" (TArray (TInteger 2) (Just [(1, 5)]))]
          )
        ]
      (strctTables !! 2) `shouldBe` M.fromList
        [ ( "str_inner"
          , [FieldEntry "inner_arr" (TArray (TInteger 1) (Just [(1, 3)]))]
          )
        , ( "str_outer"
          , [ FieldEntry "outer_arr"
                         (TArray (TCustom "str_inner") (Just [(1, 5)]))
            ]
          )
        ]


  describe "Union tests" $ do
    it "test unions in a structure" $ do
      structTable <- getStructureTable "test/structure_table/union_struct.f"
      let expStruct = M.fromList
            [ ( "foo"
              , [ UnionEntry
                    [ [FieldEntry "bar" (TInteger 4)]
                    , [ FieldEntry "baz" (TInteger 2)
                      , FieldEntry "qux" (TInteger 8)
                      ]
                    ]
                ]
              )
            ]
      structTable `shouldBe` expStruct

    it "test size of union fields" $ do
      structTable <- getStructureTable "test/structure_table/union_struct2.f"
      let expStruct =
            M.fromList [("quux", [FieldEntry "quuz" (TCustom "foo")])]
      M.lookup "quux" structTable `shouldBe` M.lookup "quux" expStruct

  describe "Check look ups" $ do
    it "test hasEntry"
      $ testStructureTable "test/structure_table/structure3.f"
      $ \_ structTable _ -> do
          let Just test = M.lookup "foo" structTable
          hasEntry "bar" test `shouldBe` Right (TInteger 8)
          let Just grault = M.lookup "grault" structTable
          hasEntry "garply" grault `shouldBe` Right (TCustom "quuz")

    it "Union data ref"
      $ testStructureTable "test/structure_table/union_struct2.f"
      $ \_ structTable _ -> do
          let Just test = M.lookup "quux" structTable
          hasEntry "quuz" test `shouldBe` Right (TCustom "foo")
          let Just foo = M.lookup "foo" structTable
          hasEntry "baz" foo `shouldBe` Right (TInteger 2)
          hasEntry "bar" foo `shouldBe` Right (TInteger 4)

    it "Get type of nested data reference expression"
      $ testStructureTable "test/structure_table/structure3.f"
      $ \pf structTable st -> do
          let expr =
                head
                  [ e
                  | e@ExpDataRef{} <-
                    universeBi pf :: [Expression (Analysis ())]
                  ]
          -- check looking up the expression succeeds and gives the correct type
          typeOf structTable st expr
            `shouldBe` Right (TCharacter (CharLenInt 10) 1)

    it "Get type of union data reference expression"
      $ testStructureTable "test/structure_table/union_struct3.f"
      $ \pf structTable st -> do
          let expr =
                head
                  [ e
                  | e@ExpDataRef{} <-
                    universeBi pf :: [Expression (Analysis ())]
                  ]
          typeOf structTable st expr
            `shouldBe` Right (TCharacter (CharLenInt 13) 1)

    it "Get combination of data references and subscripts" $ do
      pf <- getTestProgramAnalysis "test/structure_table/structure4.f"
      let pus = allPU pf
          logics
            :: [  ProgramUnit (Analysis ())
               -> StructureTable
               -> SymbolTable
               -> IO ()
               ]
          logics = map
            (\x pu structTable st -> do
              let expr = head
                    [ e
                    | (StExpressionAssign _ _ e _) <-
                      universeBi pu :: [Statement (Analysis ())]
                    ]
              typeOf structTable st expr `shouldBe` x
            )
            [ Right (TInteger 1)
            , Right (TInteger 2)
            , Right (TArray (TInteger 1) (Just [(1, 3)]))
            ]
      mapM_ (uncurry testStructureTablePU) $ zip pus logics

    it "Character substrings" $ do
      pf <- getTestProgramAnalysis "test/structure_table/structure5.f"
      let pus     = allPU pf
          results = [Right (TCharacter (CharLenInt 1) 1), Right (TLogical 2)]
          logic res pu strt symt = do
            let expr = head
                  [ e
                  | (StExpressionAssign _ _ e _) <-
                    universeBi pu :: [Statement (Analysis ())]
                  ]
            typeOf strt symt expr `shouldBe` res
      mapM_ (uncurry testStructureTablePU) $ zip pus (map logic results)
