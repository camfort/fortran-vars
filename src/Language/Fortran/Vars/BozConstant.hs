module Language.Fortran.Vars.BozConstant
  ( resolveBozConstant
  , bozToInt
  , bozToInt2
  , bozToInt4
  , bozToInt8
  )
where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                , toLower
                                                )
import qualified Data.Map                      as M
import           Numeric                        ( readInt
                                                , showIntAtBase
                                                )
import           Text.Read                      ( ReadS )

import           Language.Fortran.Vars.Types
                                                ( SymbolTableEntry(..)
                                                , Type(..)
                                                , SemType(..)
                                                , Kind
                                                , ExpVal(..)
                                                , SymbolTable
                                                )

-- | BozDecomposed is constructed with:
--   - String that represents the BOZ constant as it is in code, e.g. "'1111'x"
--   - String that represents digits without number system and lowercased, i.e. "ff1e" in "'Ff1E'x"
--   - Char that represents number system, i.e. 'x' in "'1111'x"
--   - Integer that represents number system, i.e. 16 in "'1111'x"
--   - String that represents binary translation of BOZ constant as it is.
--     It doesn't take into account any truncations nor overflows
data BozDecomposed = BozDecomposed String String Char Int String
  deriving Show

parseBozDecomposed :: ExpVal -> BozDecomposed
parseBozDecomposed (Boz bozStr) = BozDecomposed bozStr
                                                digits
                                                numsysChar
                                                numsysInt
                                                binary
 where
  digits     = getDigits bozStr
  numsysChar = if head bozStr `elem` "boxz" then head bozStr else last bozStr
  numsysInt  = case numsysChar of
    'b' -> 2
    'o' -> 8
    'x' -> 16
    'z' -> 16
    _   -> error
      (numsysChar
      : " is not supported BOZ specifier.\
                                                \ Invalid fortran syntax"
      )
  binary = toBinaryString digits numsysInt
parseBozDecomposed _ = error "ExpVal is not a BOZ constant"

resolveBozConstant' :: SymbolTable -> String -> BozDecomposed -> ExpVal
resolveBozConstant' symTable assignSymbol bozDecomposed =
  let entry = M.lookup assignSymbol symTable
  in
    case entry of
      Just (SVariable (TInteger kind) _) ->
        resolveBozConstantInContext bozDecomposed kind
      Just (SVariable ty _) ->
        error
          $  assignSymbol
          <> " is a "
          <> show ty
          <> "\nBOZ constants can only be resolved in an INTEGER context"
      Just _ -> error
        (assignSymbol
        ++ " is not a scalar variable. \
                                          \Invalid fortran sytax"
        )
      Nothing -> error
        (assignSymbol
        ++ " could not be found. \
                                          \Invalid fortran syntax"
        )

resolveBozConstantInContext :: BozDecomposed -> Kind -> ExpVal
resolveBozConstantInContext (BozDecomposed _ _ _ _ binary) kind =
  let allowedBinaryLength = kind * 8
      maxBinaryValue      = 2 ^ (allowedBinaryLength - 1) - 1
      minBinaryValue      = (-1) * 2 ^ (allowedBinaryLength - 1)
      truncatedBinary = reverse . take allowedBinaryLength . reverse $ binary
      decimal             = numsysStringToDecimal truncatedBinary 2
      overflow            = decimal - maxBinaryValue
  in  if overflow > 0 then Int (minBinaryValue + overflow - 1) else Int decimal

getDigits :: String -> String
getDigits bozStr = [ toLower c | c <- digits ]
 where
  digits =
    foldr (\l acc -> if l `elem` "'boxz" then acc else l : acc) [] bozStr

toBinaryString :: String -> Int -> String
toBinaryString digits fromNumsys = decimalToBinaryString decimal
  where decimal = numsysStringToDecimal digits fromNumsys

numsysStringToDecimal :: String -> Int -> Int
numsysStringToDecimal digits numsys = decimal
 where
  numsysValidFunction = (`elem` map intToDigit [0 .. (numsys - 1)])
  numsysReader = readInt numsys numsysValidFunction digitToInt :: ReadS Int
  ((decimal, _) : _) = numsysReader digits

decimalToBinaryString :: Int -> String
decimalToBinaryString decimal = showIntAtBase 2 intToDigit decimal ""

-- | Given 'SymbolTable', contextual symbol name and BOZ Constant
-- ('ExpVal' constructed with Boz String), resolve BOZ Constant considering
-- contextual symbol.
--
-- Currently, it only resolves BOZ Constants in context of INTEGER.
resolveBozConstant :: SymbolTable -> String -> ExpVal -> ExpVal
resolveBozConstant symTable assignSymbol boz@(Boz _) =
  resolveBozConstant' symTable assignSymbol (parseBozDecomposed boz)
resolveBozConstant _ _ _ = error "Can only resolve ExpVal Boz"


-- Convert BOZ string to integer of specific kind
bozToInt :: Int -> ExpVal -> ExpVal
bozToInt kind boz = resolveBozConstantInContext (parseBozDecomposed boz) kind

-- Convert BOZ string to integer*2
bozToInt2 :: ExpVal -> ExpVal
bozToInt2 = bozToInt 2

-- Convert BOZ string to integer*4
bozToInt4 :: ExpVal -> ExpVal
bozToInt4 = bozToInt 4

-- Convert BOZ string to integer*8
bozToInt8 :: ExpVal -> ExpVal
bozToInt8 = bozToInt 8
