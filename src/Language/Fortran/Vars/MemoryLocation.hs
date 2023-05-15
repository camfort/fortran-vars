module Language.Fortran.Vars.MemoryLocation
  ( getLocation
  , generateLinearizedIndexRange
  , getStartLocation
  )
where

import qualified Data.Foldable                 as Foldable
import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList(..)
                                                , Expression(..)
                                                , Index(..)
                                                , Name
                                                , Value(..)
                                                )

import           Language.Fortran.Vars.Eval     ( eval' )
import           Language.Fortran.Vars.Kind     ( toInt
                                                , getTypeKind
                                                )
import           Language.Fortran.Vars.Range    ( Range )
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , SemType(..)
                                                , Location
                                                , Offset
                                                , SymbolTable
                                                , Dim(..), Dimensions
                                                , dimensionsToTuples
                                                )

isIxSingle :: Index a -> Bool
isIxSingle IxSingle{} = True
isIxSingle IxRange{}  = False

linearizedIndex :: [Int] -> [(Int, Int)] -> Int
linearizedIndex indices dimensions =
  -- The normalized index starts at 0
  let normalizedIndices =
          zipWith (curry (\(i, (lower, _)) -> i - lower)) indices dimensions
      dimSizes    = map (\(lower, upper) -> upper - lower + 1) dimensions
      leadingDims = calcLeadingDimensions dimSizes
  in  foldl' (\acc (i, ld) -> acc + i * ld)
             0
             (zip normalizedIndices leadingDims)
 where
  calcLeadingDimensions :: [Int] -> [Int]
  calcLeadingDimensions []       = []
  calcLeadingDimensions dimSizes = calc [1] dimSizes
   where
    calc _   []      = []
    calc acc [_    ] = reverse acc
    calc acc (h : t) = calc ((head acc * h) : acc) t

-- | Given only single indices return the 'Range' in memory that
-- these indices point to.
generateLinearizedIndexRange
    :: (Functor t, Foldable t) => [Int] -> Int -> t (Dim Int) -> Int -> Range
generateLinearizedIndexRange intIndices start dims kind =
  let offset = linearizedIndex intIndices dims' * kind
  in  (start + offset, start + offset + kind - 1)
  where
    -- TODO Ideally, we stay in our foldable for as long as possible. Shift this
    -- into 'linearizedIndex' and try.
    dims' = Foldable.toList $ fmap (\(Dim lb ub) -> (lb, ub)) dims

findBlockOffset :: SymbolTable -> Name -> Offset -> Location
findBlockOffset symTable symbol offset = case M.lookup symbol symTable of
  Just (SVariable _ (blockName, start)) -> (blockName, start + offset)
  Just entry -> error $ symbol ++ " is not a variable - " ++ show entry
  Nothing ->
    error
      $  "Unable to find location for symbol "
      ++ symbol
      ++ " at offset "
      ++ show offset

calculateOffset
  :: Data a => SymbolTable -> Name -> [Index (Analysis a)] -> Maybe Int
-- array index c(2,4)
calculateOffset symTable symbol indices@(IxSingle{} : _) =
  let Just entry = M.lookup symbol symTable
  in
    case entry of
      SVariable (TArray ty dims) _ ->
        case dimensionsToTuples dims of
          Nothing -> error "expected a static array, got dynamic"
          Just dims' ->
            let
              ixSingles    = takeWhile isIxSingle indices
              Just kind    = getTypeKind ty
              arrayIndices = either (const Nothing) Just
                $ traverse toIndices ixSingles
               where
                toIndices (IxSingle _ _ _ expr) = toInt <$> eval' symTable expr
                toIndices _ = error "toIndices: unexpected input"
            in
              (\x -> linearizedIndex x dims' * kind) <$> arrayIndices
      _ -> error "Only array-typed VariableEntries are expected at this point"

-- substring c(:5)
calculateOffset _ _ (IxRange _ _ Nothing _ _ : _) = Just 0

-- substring c(5:)
calculateOffset symTable _ (IxRange _ _ (Just lowerIndex) _ _ : _) =
  let val = eval' symTable lowerIndex
  in  either (const Nothing) (\x -> Just $ toInt x - 1) val
calculateOffset _ _ _ = error "calculateOffset: invalid index"

-- | Given a 'SymbolTable' and some 'Expression' (which is assumed to have been predetermined
-- to be of some variable type), return the 'Location' that the variable in question will be
-- located in memory
getLocation
  :: Data a => SymbolTable -> Expression (Analysis a) -> Maybe Location
-- variable
getLocation symTable e@(ExpValue _ _ (ValVariable _)) =
  Just $ findBlockOffset symTable (srcName e) 0
-- array index c(2,4)
-- substring c(5:10)
getLocation symTable (ExpSubscript _ _ e@ExpValue{} (AList _ _ indices)) =
  let symbol = srcName e
      offset = calculateOffset symTable symbol indices
  in  findBlockOffset symTable symbol <$> offset
-- array index and substring c(2,4)(1:20)
getLocation symTable (ExpSubscript _ _ (ExpSubscript _ _ e@ExpValue{} (AList _ _ indices)) (AList _ _ subs))
  = let symbol = srcName e
        offset =
            (+)
              <$> calculateOffset symTable symbol indices
              <*> calculateOffset symTable symbol subs
    in  findBlockOffset symTable symbol <$> offset
-- array within common block with dimensions declaration: common /block/ a, b(10)
getLocation symTable (ExpFunctionCall _ _ e@ExpValue{} _) =
  Just $ findBlockOffset symTable (srcName e) 0
getLocation _ _ = Nothing

-- | Given a 'SymbolTable' and some 'Expression' (which is assumed to have been
-- predetermined to be of some variable type), return the start 'Location' that
-- the variable in question will be located in memory.
-- Start 'Location' is the begining of greater data structure that a variable
-- belongs to e.g. start 'Location' of c(20) is 'Location' of c
getStartLocation :: Data a => SymbolTable -> Expression (Analysis a) -> Location
-- variable
getStartLocation symTable e@(ExpValue _ _ (ValVariable _)) =
  findBlockOffset symTable (srcName e) 0
-- dimensions specification within COMMON c(10,20)
getStartLocation symTable (ExpSubscript _ _ e@ExpValue{} _) =
  findBlockOffset symTable (srcName e) 0
-- dimensions specification within COMMON c(10,20)
--   (vars with nonstandard kind declared after COMMON block)
getStartLocation symTable (ExpFunctionCall _ _ e@ExpValue{} _) =
  findBlockOffset symTable (srcName e) 0
getStartLocation _ _ = error "getStartLocation : Not a variable expression"
