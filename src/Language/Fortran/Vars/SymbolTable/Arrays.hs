module Language.Fortran.Vars.SymbolTable.Arrays where

import Language.Fortran.AST

import Language.Fortran.Vars.Eval ( eval' )
import Language.Fortran.Vars.Types ( ExpVal(..), SymbolTable, Dimensions )

import Control.Monad.Except

resolveDims
    :: SymbolTable -> [DimensionDeclarator a] -> Maybe Dimensions
resolveDims = undefined

resolveDimsAssumedShape
    :: SymbolTable -> [DimensionDeclarator a] -> Either String [Int]
resolveDimsAssumedShape symt = traverse go
  where
    go (DimensionDeclarator _ _ mlb mub) =
        case mub of
          Just{} ->
            throwError "assumed-shape array can't have a dimension with an upper bound"
          Nothing ->
            case mlb of
              Nothing -> pure 1
              Just lbExpr ->
                case eval' symt lbExpr of
                  Right (Int lb) -> pure lb
                  Right{} ->
                    throwError "wrong type for array dimension lower bound"
                  Left e ->
                    throwError $ "couldn't evaluate assumed-shape array dimension lower bound: " <> e

{-


-- | Evaluate array dimension information.
--
-- There are essentially three possible return values:
--
--   * static-size array, where all dimensions are fully defined
--   * assumed-size array, where final dimension is dynamic
--   * other (e.g. assumed-shape, unsupported), all information discarded
resolveDims
  :: SymbolTable -> [DimensionDeclarator a] -> Maybe Dimensions
resolveDims symt = foldr go 
  where
    go a b = b
    coerceDimensionList . fromMaybe [] . sequence . map (resolveDim symt)

-- `Nothing` means totally invalid.
resolveDim
  :: SymbolTable -> DimensionDeclarator a -> Maybe (Dim Int)
resolveDim symt (DimensionDeclarator _ _ mlb mub) =
    case ub of
      Just (ExpValue _ _ ValStar) -> do
        -- final dimension is @*@: assumed-size
        -- note that we can't represent a lower bound here due to the data
        -- type. oops. TODO
        pure $ DimensionDynamic mlbv
      _ -> do
        case mub of
          Nothing -> Nothing
          Just ub -> do
            ubv <- resolveDimBound symt ub
            pure $ DimensionStatic  mlbv ubv
  where
    mlbv = case mlb of Nothing -> Nothing; Just lb -> resolveDimBound symt lb

-- note that we don't handle @*@ here. do that manually, for the last dimension
-- (and only for the upper bound -- TODO I think? check standard). if it's not
-- the last dimension, it's not an assumed-size array and so we can throw
-- everything out
resolveDimBound
  :: SymbolTable -> Expression a -> Maybe Int
resolveDimBound symt = \case
  ExpValue _ _ (ValVariable name) ->
    case M.lookup name symt of
      Just (SParameter _ (Int i)) -> Just i
      _                           -> Nothing
  e ->
    case eval' symt e of
      Right (Int i) -> Just i
      _             -> Nothing

-}
