{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.Vars.Assignments
  ( allAssignStmts
  )
where

import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Data.Generics.Uniplate.Data    ( universeBi )

import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.AST           ( ProgramUnit
                                                , Statement(..)
                                                , DataGroup(..)
                                                , Expression(..)
                                                , Declarator(..)
                                                , Value(..)
                                                , aStrip
                                                )
import           Language.Fortran.Util.Position ( Spanned(..) )

import           Language.Fortran.Extras.Encoding
                                                ( pprint77l )
import           Language.Fortran.Vars.StructureTable
                                                ( collectStructures )
import           Language.Fortran.Vars.SymbolTable
                                                ( collectSymbols )
import           Language.Fortran.Vars.Types
                                                ( SymbolTable
                                                , StructureTable
                                                , SymbolTableEntry(..)
                                                , Dimensions
                                                , Type(..)
                                                , SemType(..)
                                                , TypeError(..)
                                                , typeError
                                                )
import           Language.Fortran.Vars.TypeCheck
                                                ( typeOf )

-- | Method to retrieve the type of the lhs and expression on the rhs of all
-- assign like statements: expression assign, parameter, data and declarations
-- It returns the type of the lhs due to the expansion of array types to scalar
-- types in datagroups and declarations and returns TypeError's for the
-- expressions it can't calculate.
allAssignStmts
  :: forall a
   . Data a
  => ProgramUnit (Analysis a)
  -> [Either TypeError (Type, Expression (Analysis a))]
allAssignStmts pu =
  let
    symt = collectSymbols pu
    strt = collectStructures symt pu
  in
    [ (, e) <$> ty
    | StExpressionAssign _ _ v e <- universeBi pu :: [Statement (Analysis a)]
    , let ty = typeOf strt symt v
    ]
    <> [ (, e) <$> ty
       | StParameter _ _ decls <- universeBi pu :: [Statement (Analysis a)]
       , DeclVariable _ _ v _ (Just e) <- aStrip decls
       , let ty = typeOf strt symt v
       ]
    <> [ res
       | StData _ _ groups <- universeBi pu :: [Statement (Analysis a)]
       , res               <- dataGroups strt symt (aStrip groups)
       ]
    <> [ res
       | StDeclaration _ _ _ _ decls <-
         universeBi pu :: [Statement (Analysis a)]
       , res <- declarators strt symt $ aStrip decls
       ]

-- | Deal with data groups
dataGroups
  :: StructureTable
  -> SymbolTable
  -> [DataGroup (Analysis a)]
  -> [Either TypeError (Type, Expression (Analysis a))]
dataGroups strt symt = concatMap f
 where
  f (DataGroup _ _ vas eas) =
    let vs         = aStrip vas
        es         = aStrip eas
        expandedVs = concatMap (expandArrays strt symt) vs
        g (Left  err) _ = Left err
        g (Right ty ) e = Right (ty, e)
    in  zipWith g expandedVs es

-- | Expands declarators to lhs type and rhs expression
declarators
  :: StructureTable
  -> SymbolTable
  -> [Declarator (Analysis a)]
  -> [Either TypeError (Type, Expression (Analysis a))]
declarators strt symt = concatMap f where
  f (DeclVariable _ _ v _ (Just e)) = pure $ (, e) <$> typeOf strt symt v
  f d@(DeclArray _ _ (ExpValue _ s (ValVariable v)) _ _ (Just (ExpInitialisation _ _ vals)))
    = case M.lookup v symt of
      Just (SVariable (TArray ty (Just dims)) _) ->
        let tys   = expandDimensions dims ty
            vals' = aStrip vals
        in  if length tys /= length vals'
              then
                pure
                . Left
                . typeError s
                . mconcat
                $ [ "Length of lhs and rhs in declarator do not match: "
                  , ": "
                  , pprint77l d
                  ]
              else fmap Right $ zip (expandDimensions dims ty) $ aStrip vals
      _ ->
        pure
          .  Left
          .  typeError s
          $  "Unexpected lhs in array declaration at: "
          <> pprint77l d
  f _ = []  -- All other cases have no initial values

-- | Expands arrays to give a list of types given the length of an array
expandArrays
  :: StructureTable
  -> SymbolTable
  -> Expression (Analysis a)
  -> [Either TypeError Type]
expandArrays strt symt e = case e of
  ExpValue _ _ (ValVariable var) -> case M.lookup var symt of
    Just (SVariable (TArray ty (Just dims)) _) ->
      expandDimensions dims (Right ty)
    Just (SVariable ty _) -> [Right ty]
    _ ->
      pure
        .  Left
        .  typeError (getSpan e)
        $  "Got unexpected lhs type: "
        <> pprint77l e
  _ -> pure $ typeOf strt symt e

-- | Function to expand dimensions into appropriate number of types for use in
-- other expand functions
expandDimensions :: Dimensions -> a -> [a]
expandDimensions dims =
  replicate (foldl' (\acc (x, y) -> abs (y - x + 1) * acc) 1 dims)
