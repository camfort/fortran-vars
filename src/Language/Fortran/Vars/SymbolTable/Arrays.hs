module Language.Fortran.Vars.SymbolTable.Arrays where

import Language.Fortran.AST

import Language.Fortran.Vars.Eval ( eval' )
import Language.Fortran.Vars.Types ( ExpVal(..), SymbolTable, Dim(..), Dims(..), Dimensions )

import Control.Monad.Except
--import Data.List.NonEmpty ( NonEmpty( (:|) ) )

resolveDims
    :: SymbolTable -> [DimensionDeclarator a] -> Maybe Dimensions
resolveDims symt dds
  -- We assume array type from a quick look at the dimension declarators.
  | any dimDeclLooksLikeAssumedSize  dds =
      case resolveDimsAssumedSize symt dds of
        Left _err -> Nothing -- discard errors/warnings :(
        Right (Nothing, x) -> Just $ DimsAssumedSize Nothing x
        -- resolveDimsAssumedSize can't return an empty list. Clumsy code means
        -- we don't prove this in types.
        Right (Just (a:as), x) -> Just $ DimsAssumedSize (Just (a :| as)) x
        Right (Just [], _x) -> error "impossible"
  | any dimDeclLooksLikeAssumedShape dds =
      case resolveDimsAssumedShape symt dds of
        Left _err -> Nothing -- discard errors/warnings :(
        Right [] -> error "empty DimensionDeclarator list (should not be parseable)"
        Right (a:as) -> Just $ DimsAssumedShape $ a :| as
  | otherwise =
      case resolveDimsExplicitShape symt dds of
        Left _err -> Nothing -- discard errors/warnings :(
        Right [] -> error "empty DimensionDeclarator list (should not be parseable)"
        Right (a:as) -> Just $ DimsExplicitShape $ a :| as

-- | Assumed-size arrays have the special 'ValStar' upper bound (whereas
--   explicit-shape and assumed-shape arrays never do).
dimDeclLooksLikeAssumedSize :: DimensionDeclarator a -> Bool
dimDeclLooksLikeAssumedSize = \case
  DimensionDeclarator _ _ _ (Just (ExpValue _ _ ValStar)) -> True
  _ -> False

-- | Assumed-shape arrays have no upper bounds (whereas explicit-shape and
--   assumed-size arrays always do).
dimDeclLooksLikeAssumedShape :: DimensionDeclarator a -> Bool
dimDeclLooksLikeAssumedShape = \case
  DimensionDeclarator _ _ _ Nothing -> True
  _ -> False

evalStaticDimBoundExpr :: SymbolTable -> Expression a -> Either String Int
evalStaticDimBoundExpr symt expr =
    case eval' symt expr of
      Right (Int val) -> pure val
      Right{} -> throwError $ "wrong type for array dimension bound"
      Left err -> throwError $ "error evaluating array dimension bound expression: "<>err

-- | Returns @'Right' 'Nothing'@ for dynamic bounds (e.g. which use dummy vars).
evalDynamicDimBoundExpr :: SymbolTable -> Expression a -> Either String (Maybe Int)
evalDynamicDimBoundExpr symt expr =
    case eval' symt expr of
      Right (Int val) -> pure $ Just val
      Right{} -> throwError $ "wrong type for array dimension bound"
      Left{} -> pure Nothing

resolveDimsExplicitShape
    :: SymbolTable -> [DimensionDeclarator a] -> Either String [Dim (Maybe Int)]
resolveDimsExplicitShape symt = traverse (resolveDimExplicitShape symt)

resolveDimExplicitShape
    :: SymbolTable -> DimensionDeclarator a -> Either String (Dim (Maybe Int))
resolveDimExplicitShape symt (DimensionDeclarator _ _ mlb mub) =
    case mub of
      Nothing -> throwError "explicit-shape array must have an upper bound for every dimension"
      Just ubExpr -> do
        lb <- case mlb of
                Nothing -> pure $ Just 1
                Just lbExpr -> evalDynamicDimBoundExpr symt lbExpr
        ub <- evalDynamicDimBoundExpr symt ubExpr
        pure $ Dim lb ub

resolveDimsAssumedShape
    :: SymbolTable -> [DimensionDeclarator a] -> Either String [Maybe Int]
resolveDimsAssumedShape symt = traverse go
  where
    go (DimensionDeclarator _ _ mlb mub) =
        case mub of
          Just{} ->
            throwError "assumed-shape array can't have a dimension with an upper bound"
          Nothing ->
            case mlb of
              Nothing -> pure $ Just 1
              Just lbExpr -> evalDynamicDimBoundExpr symt lbExpr

resolveDimsAssumedSize
    :: SymbolTable -> [DimensionDeclarator a]
    -> Either String (Maybe [Dim (Maybe Int)], Maybe Int)
resolveDimsAssumedSize symt = \case
  []   -> throwError "resolveDimsAssumedSize: array can't have zero dimensions"
  d:[] ->
    case resolveDimStar symt d of
      Left err -> Left err
      Right a -> Right (Nothing, a)
  ds   ->
    case go [] ds of
      Left err -> Left err
      Right (l, r) -> Right (Just l, r)
  where
    go
        :: [Dim (Maybe Int)] -> [DimensionDeclarator a]
        -> Either String ([Dim (Maybe Int)], Maybe Int)
    go ds = \case
      []     -> Right (reverse ds, Nothing)
      dd:[]  ->
        case resolveDimStar symt dd of
          Left  err -> Left err
          Right d   -> Right (ds, d)
      dd:dds ->
        case resolveDimExplicitShape symt dd of
          Left  err -> Left err
          Right d   -> go (d:ds) dds

resolveDimStar
    :: SymbolTable -> DimensionDeclarator a -> Either String (Maybe Int)
resolveDimStar symt (DimensionDeclarator _ _ mlb mub) = do
    lb <- case mlb of
            Nothing -> pure $ Just 1
            Just lbExpr -> evalDynamicDimBoundExpr symt lbExpr
    () <- resolveDimBoundStar mub
    pure lb

resolveDimBoundStar :: Maybe (Expression a) -> Either String ()
resolveDimBoundStar = \case
  Just (ExpValue _ _ ValStar) -> pure ()
  Just{}  -> throwError "resolveDimBoundStar: expression wasn't a ValStar"
  Nothing -> throwError "resolveDimBoundStar: upper bound must be present"
