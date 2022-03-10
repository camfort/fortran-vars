module StorageTableSpec where

import           Test.Hspec

import           Language.Fortran.Util.Files
                                                ( flexReadFile )
import           Language.Fortran.Extras.ProgramFile
                                                ( versionedProgramFile )
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )

import           Language.Fortran.AST           ( Name
                                                , ProgramUnitName(..)
                                                )
import           Language.Fortran.Version   ( FortranVersion(..) )
import           Language.Fortran.Analysis      ( initAnalysis )

import           Language.Fortran.Vars ( programFileModel )
import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , Location
                                                , MemoryBlock(..)
                                                , MemoryBlockName
                                                , ProgramUnitModel
                                                , StorageClass(..)
                                                , StorageTable
                                                , SymbolTable
                                                , Type(..)
                                                , SemType(..)
                                                , CharacterLen(..)
                                                )
import           Language.Fortran.Vars.CommonLayout
                                                ( getCommonLayout
                                                , getFlagType
                                                )

getModel :: String -> ByteString -> String -> ProgramUnitModel
getModel p c n =
  let pf  = initAnalysis $ versionedProgramFile Fortran77Legacy p c
      pfm = programFileModel pf
  in  fromMaybe (error $ "Failed to lookup unit: " ++ n)
        $ M.lookup (Named n) pfm

blockSizeOf :: MemoryBlockName -> StorageTable -> Int
blockSizeOf name storageTable =
  fromJust $ M.lookup name storageTable >>= blockSize

locationOf :: Name -> SymbolTable -> Location
locationOf name symTable = case M.lookup name symTable of
  Just (SVariable _ loc) -> loc
  _                      -> error (show name ++ " is not a VariableEntry")

blockNameOf :: Name -> SymbolTable -> MemoryBlockName
blockNameOf name symTable =
  let (blockName, _) = locationOf name symTable in blockName

offsetOf :: Name -> SymbolTable -> Int
offsetOf name symTable = let (_, offset) = locationOf name symTable in offset

storageClassOf :: MemoryBlockName -> StorageTable -> StorageClass
storageClassOf name storageTable =
  storageClass . fromJust $ M.lookup name storageTable

storageClassOfVar :: Name -> ProgramUnitModel -> StorageClass
storageClassOfVar name (symTable, storageTable) =
  storageClassOf (blockNameOf name symTable) storageTable

variablesOf :: MemoryBlockName -> StorageTable -> [Name]
variablesOf name storageTable =
  variables . fromJust $ M.lookup name storageTable

spec :: Spec
spec = do

  describe "StorageTable: default " $ do

    let path     = "test/memory_block.f"
        unitName = "foo"

    it "Single Variable" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName
      let var                      = "a1"
      -- use the name of the variable as the default block name
      blockNameOf var symTable `shouldBe` "a1"
      offsetOf var symTable `shouldBe` 0
      blockSizeOf var storageTable `shouldBe` 4
      -- default storage class is Unspecified
      storageClassOf "a1" storageTable `shouldBe` Unspecified

    it "Array Variable" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName
      let var                      = "a2"
      -- use the name of the variable as the default block name
      blockNameOf var symTable `shouldBe` "a2"
      offsetOf var symTable `shouldBe` 0
      blockSizeOf var storageTable `shouldBe` (3 * 4 * 4)
      -- default storage class is Unspecified
      storageClassOf "a2" storageTable `shouldBe` Unspecified

  describe "Storage Class" $ do

    let path     = "test/storage_class.f"
        unitName = "foo"

    it "Default Unspecified" $ do
      contents <- flexReadFile path
      let st  = getModel path contents unitName
      let var = "a"
      storageClassOfVar var st `shouldBe` Unspecified

    it "Static" $ do
      contents <- flexReadFile path
      let st = getModel path contents unitName
      storageClassOfVar "b1" st `shouldBe` Static
      storageClassOfVar "b2" st `shouldBe` Static

    it "Automatic" $ do
      contents <- flexReadFile path
      let st = getModel path contents unitName
      storageClassOfVar "c1" st `shouldBe` Automatic
      storageClassOfVar "c2" st `shouldBe` Automatic

    it "Common" $ do
      contents <- flexReadFile path
      let st = getModel path contents unitName
      storageClassOfVar "d1" st `shouldBe` Common
      storageClassOfVar "d2" st `shouldBe` Common

  describe "Equivalence" $ do

    let path     = "test/equivalence.f"
        unitName = "foo"

    it "Variable and single dimensional array" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName

      blockNameOf "var" symTable `shouldBe` "array"
      blockNameOf "array" symTable `shouldBe` "array"

      offsetOf "array" symTable `shouldBe` 0
      offsetOf "var" symTable `shouldBe` 56

      variablesOf "array" storageTable `shouldBe` ["array", "var"]

    it "Memory block merging:  equal sizes, different offsets" $ do
      contents <- flexReadFile path
      let (symt, storageTable) = getModel path contents unitName

      blockNameOf "b" symt `shouldBe` "b"
      blockNameOf "c" symt `shouldBe` "b"

      offsetOf "b" symt `shouldBe` 0
      offsetOf "c" symt `shouldBe` (4 * 5)

      variablesOf "b" storageTable `shouldBe` ["b", "c"]

    it "Memory block merging:  equal offsets, different sizes" $ do
      contents <- flexReadFile path
      let (symt, storageTable) = getModel path contents unitName

      blockNameOf "d" symt `shouldBe` "d"
      blockNameOf "e" symt `shouldBe` "d"

      offsetOf "d" symt `shouldBe` 0
      offsetOf "e" symt `shouldBe` 0

      variablesOf "d" storageTable `shouldBe` ["d", "e"]

    it "Three way equivalence - 1" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "aa" symt `shouldBe` "cc"
      blockNameOf "bb" symt `shouldBe` "cc"
      blockNameOf "cc" symt `shouldBe` "cc"

      offsetOf "aa" symt `shouldBe` 0
      offsetOf "bb" symt `shouldBe` 3
      offsetOf "cc" symt `shouldBe` 0

      variablesOf "cc" storaget `shouldBe` ["cc", "aa", "bb"]

    it "Three way equivalence - 2" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "sndbuff" symt `shouldBe` "sndbuff"
      blockNameOf "sndbbid1" symt `shouldBe` "sndbuff"
      blockNameOf "sndbbid2" symt `shouldBe` "sndbuff"

      offsetOf "sndbuff" symt `shouldBe` 0
      offsetOf "sndbbid1" symt `shouldBe` 2
      offsetOf "sndbbid2" symt `shouldBe` 6

      variablesOf "sndbuff" storaget
        `shouldBe` ["sndbuff", "sndbbid1", "sndbbid2"]


    it "List of equivalences" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "r1" symt `shouldBe` "r1"
      blockNameOf "r2" symt `shouldBe` "r1"
      blockNameOf "r3" symt `shouldBe` "r1"
      variablesOf "r1" storaget `shouldBe` ["r1", "r2", "r3"]
      blockNameOf "i1" symt `shouldBe` "i1"
      blockNameOf "i2" symt `shouldBe` "i1"
      blockNameOf "i3" symt `shouldBe` "i1"
      variablesOf "i1" storaget `shouldBe` ["i1", "i2", "i3"]

    it "Multidimensional array" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "threedim" symt `shouldBe` "threedim"
      blockNameOf "twodim" symt `shouldBe` "threedim"
      offsetOf "threedim" symt `shouldBe` 0
      offsetOf "twodim" symt `shouldBe` 400
      variablesOf "threedim" storaget `shouldBe` ["threedim", "twodim"]

      blockNameOf "fivedim" symt `shouldBe` "sevendim"
      blockNameOf "sevendim" symt `shouldBe` "sevendim"
      offsetOf "sevendim" symt `shouldBe` 0
      offsetOf "fivedim" symt `shouldBe` 4444444
      variablesOf "sevendim" storaget `shouldBe` ["sevendim", "fivedim"]


    it "Substring" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "exchange_code" symt `shouldBe` "inbuf"
      blockNameOf "price" symt `shouldBe` "inbuf"
      offsetOf "exchange_code" symt `shouldBe` 9
      offsetOf "price" symt `shouldBe` 22
      variablesOf "inbuf" storaget
        `shouldBe` ["inbuf", "exchange_code", "price"]


    it "Array and substring" $ do
      contents <- flexReadFile path
      let (symt, storaget) = getModel path contents unitName
      blockNameOf "tick" symt `shouldBe` "control_switches"
      blockNameOf "control_switches" symt `shouldBe` "control_switches"
      offsetOf "tick" symt `shouldBe` 7
      offsetOf "control_switches" symt `shouldBe` 0
      variablesOf "control_switches" storaget
        `shouldBe` ["control_switches", "tick"]

    it "Repeated equivalences" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName

      blockNameOf "p6buf" symTable `shouldBe` "p6buf"
      blockNameOf "p6buf2" symTable `shouldBe` "p6buf"
      blockNameOf "p6uuid" symTable `shouldBe` "p6buf"

      offsetOf "p6buf" symTable `shouldBe` 0
      offsetOf "p6buf2" symTable `shouldBe` 0
      offsetOf "p6uuid" symTable `shouldBe` 0

      variablesOf "p6buf" storageTable `shouldBe` ["p6buf", "p6buf2", "p6uuid"]

  describe "Common" $ do

    let path      = "test/common.f"
        unitName1 = "suba"
        unitName2 = "subb"
        unitName3 = "subc"
        unitName4 = "subd"

    it "Basic common" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName1
          commonName               = "reqnamecomn"
          commonNameEncoding       = "/" ++ commonName ++ "/"

      blockNameOf "reqname_a" symTable `shouldBe` commonNameEncoding
      blockNameOf "rcode_a" symTable `shouldBe` commonNameEncoding

      offsetOf "reqname_a" symTable `shouldBe` 0
      offsetOf "rcode_a" symTable `shouldBe` 448

      variablesOf commonNameEncoding storageTable
        `shouldBe` ["reqname_a", "rcode_a"]
      storageClassOf commonNameEncoding storageTable `shouldBe` Common

    it "Common and equivalance" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName2
          commonName               = "reqnamecomn"
          commonNameEncoding       = "/" ++ commonName ++ "/"

      blockNameOf "reqname_b" symTable `shouldBe` commonNameEncoding
      blockNameOf "rcode_b" symTable `shouldBe` commonNameEncoding
      blockNameOf "ext" symTable `shouldBe` commonNameEncoding

      offsetOf "reqname_b" symTable `shouldBe` 0
      offsetOf "rcode_b" symTable `shouldBe` 448
      offsetOf "ext" symTable `shouldBe` 448

      variablesOf commonNameEncoding storageTable
        `shouldBe` ["reqname_b", "rcode_b", "ext"]
      storageClassOf commonNameEncoding storageTable `shouldBe` Common

    it "Common and variable with the same name" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName3
          variableName             = "common_block_name"
          commonNameEncoding       = "/" ++ variableName ++ "/"

      blockNameOf "a" symTable `shouldBe` commonNameEncoding
      blockNameOf variableName symTable `shouldBe` commonNameEncoding
      blockNameOf "b" symTable `shouldBe` commonNameEncoding

      offsetOf "a" symTable `shouldBe` 0
      offsetOf variableName symTable `shouldBe` 4
      offsetOf "b" symTable `shouldBe` 8

      variablesOf commonNameEncoding storageTable
        `shouldBe` ["a", variableName, "b"]

    it "Common with dimension specifiers" $ do
      contents <- flexReadFile path
      let (symTable, storageTable) = getModel path contents unitName4
          commonName_1             = "common_block_1"
          commonNameEncoding_1     = "/" ++ commonName_1 ++ "/"
          commonName_2             = "common_block_2"
          commonNameEncoding_2     = "/" ++ commonName_2 ++ "/"

      blockNameOf "a" symTable `shouldBe` commonNameEncoding_1
      blockNameOf "b" symTable `shouldBe` commonNameEncoding_1
      blockNameOf "c" symTable `shouldBe` commonNameEncoding_1

      blockNameOf "d" symTable `shouldBe` commonNameEncoding_2
      blockNameOf "e" symTable `shouldBe` commonNameEncoding_2
      blockNameOf "f" symTable `shouldBe` commonNameEncoding_2

      -- TODO: Run those tests when nonstandard kind specifiers
      --       handling is implemented
      --offsetOf "a" symTable `shouldBe` 0
      --offsetOf "b" symTable `shouldBe` 80
      --offsetOf "c" symTable `shouldBe` 320

      --offsetOf "d" symTable `shouldBe` 0
      --offsetOf "e" symTable `shouldBe` 72
      --offsetOf "f" symTable `shouldBe` 152

      variablesOf commonNameEncoding_1 storageTable `shouldBe` ["a", "b", "c"]
      variablesOf commonNameEncoding_2 storageTable `shouldBe` ["d", "e", "f"]


    it "Comman area with Structures" $ do
      contents <- flexReadFile "test/common_structs.f"
      let (symTable, storageTable) = getModel path contents "main"
          blankCommon              = "*blank_common*"
      blockNameOf "my_foo" symTable `shouldBe` blankCommon
      blockNameOf "my_foo_2" symTable `shouldBe` blankCommon
      blockNameOf "qux" symTable `shouldBe` blankCommon

      offsetOf "my_foo" symTable `shouldBe` 0
      offsetOf "my_foo_2" symTable `shouldBe` 1
      offsetOf "qux" symTable `shouldBe` 2

      variablesOf blankCommon storageTable
        `shouldBe` ["my_foo", "my_foo_2", "qux"]

    it "Common Layout" $ do
      let clPath = "test/common_layout.f"
      contents <- flexReadFile clPath
      let pumA            = getModel clPath contents "suba"
      let pumB            = getModel clPath contents "subb"
      let pumC            = getModel clPath contents "subc"

      let commonNameD     = "does_not_exist"
      let commonNameA     = "cname_a"
      let commonNameB     = "cname_b"
      let commonNameC     = "cname_c"
      let commonNameCc    = "cname_cc"
      let fflag           = getFlagType "falign-commons"
      let nflag           = getFlagType "fno-align-commons"
      let dflag           = getFlagType ""

      let commonLayoutDNE = getCommonLayout pumA commonNameD fflag
      let commonLayoutAf  = getCommonLayout pumA commonNameA fflag
      let commonLayoutAn  = getCommonLayout pumA commonNameA nflag
      let commonLayoutBf  = getCommonLayout pumB commonNameB fflag
      let commonLayoutBn  = getCommonLayout pumB commonNameB nflag
      let commonLayoutCf  = getCommonLayout pumC commonNameC fflag
      let commonLayoutCn  = getCommonLayout pumC commonNameC nflag
      let commonLayoutCc  = getCommonLayout pumC commonNameCc nflag

        -- only so long as default is fno-align-commons
      getCommonLayout pumA commonNameA nflag
        `shouldBe` getCommonLayout pumA commonNameA dflag
      getCommonLayout pumB commonNameB nflag
        `shouldBe` getCommonLayout pumB commonNameB dflag
      getCommonLayout pumC commonNameC nflag
        `shouldBe` getCommonLayout pumC commonNameC dflag


      commonLayoutDNE `shouldBe` []
      commonLayoutAf
        `shouldBe` [ ("char_a", 0, TCharacter (CharLenInt 1) 1)
                   , ("int_b" , 4, TInteger 4)
                   , ("char_c", 8, TCharacter (CharLenInt 1) 1)
                   ]
      commonLayoutAn
        `shouldBe` [ ("char_a", 0, TCharacter (CharLenInt 1) 1)
                   , ("int_b" , 1, TInteger 4)
                   , ("char_c", 5, TCharacter (CharLenInt 1) 1)
                   ]
      commonLayoutBf
        `shouldBe` [ ( "char_array_a"
                     , 0
                     , TArray (TCharacter (CharLenInt 7) 1) (Just [(1, 65)])
                     )
                   , ("int_b"      , 456, TInteger 4)
                   , ("int_array_c", 456, TArray (TInteger 4) (Just [(1, 10)]))
                   ]
      commonLayoutBn
        `shouldBe` [ ( "char_array_a"
                     , 0
                     , TArray (TCharacter (CharLenInt 7) 1) (Just [(1, 65)])
                     )
                   , ("int_b"      , 455, TInteger 4)
                   , ("int_array_c", 455, TArray (TInteger 4) (Just [(1, 10)]))
                   ]
      commonLayoutCf
        `shouldBe` [ ("int_a", 0, TInteger 4)
                   , ("int_c", 4, TInteger 4)
                   , ("int_b", 8, TInteger 4)
                   ]
      commonLayoutCn
        `shouldBe` [ ("int_a", 0, TInteger 4)
                   , ("int_c", 4, TInteger 4)
                   , ("int_b", 8, TInteger 4)
                   ]
      commonLayoutCc `shouldBe` [("int_d", 0, TInteger 4)]

  describe "Dummy Argument" $ do

    let path     = "test/dummy_argument_storage.f"
        unitName = "state"

    it "Dummy variables are not in storage" $ do
      contents <- flexReadFile path
      let (_, storageTable) = getModel path contents unitName

      let varList           = variablesOf "c" storageTable
      "temp" `elem` varList `shouldBe` False
      "f" `elem` varList `shouldBe` False
      "c" `elem` varList `shouldBe` True

  describe "Common Area with Equivalence" $ do

    let path = "test/common_equivalence.f"
        run_test unitName commonName size = do
          contents <- flexReadFile path
          let (symTable, storageTable) = getModel path contents unitName
              varList                  = variablesOf commonName storageTable

          blockSizeOf commonName storageTable `shouldBe` size

          blockNameOf "var1" symTable `shouldBe` commonName
          blockNameOf "var2" symTable `shouldBe` commonName
          blockNameOf "var3" symTable `shouldBe` commonName

          "var1" `elem` varList `shouldBe` True
          "var2" `elem` varList `shouldBe` True
          "var3" `elem` varList `shouldBe` True
          storageClassOf commonName storageTable `shouldBe` Common


    it "Variables are in the common area eq1" $ run_test "eq1" "/common1/" 12
    it "Variables are in the common area eq2" $ run_test "eq2" "/common2/" 12
    it "Variables are in the common area eq3" $ run_test "eq3" "/common3/" 12
    it "Variables are in the common area eq4" $ run_test "eq4" "/common4/" 12
    it "Variables are in the common area eq5" $ run_test "eq5" "/common5/" 16

