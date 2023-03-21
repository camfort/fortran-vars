module Language.Fortran.Vars.Utils where

import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.AST
import           Language.Fortran.Vars.Types    ( SymbolTable
                                                , ExpVal(..)
                                                , Type
                                                , SemType(..)
                                                , Dim(..)
                                                , Dims(..)
                                                , Dimensions
                                                )
import           Language.Fortran.Vars.Eval     ( eval
                                                , eval'
                                                )
import           Language.Fortran.Vars.Kind     ( setTypeKind
                                                , kindOfBaseType
                                                , baseToType
                                                )

-- | Given dimenion declarators and the typespec, give ArrayTypeData evaluating
-- valid expressions for the upper and lower bound
typeSpecToArrayType
  :: SymbolTable
  -> [DimensionDeclarator (Analysis a)]
  -> TypeSpec (Analysis a)
  -> Type
--typeSpecToArrayType st dims tySpec = TArray scalarTy $ foldr go DimensionsEnd dims
typeSpecToArrayType st dims tySpec =
    case foldr go [] dims of
      [] -> error "invalid array spec: zero dimensions"
      d:ds -> TArray scalarTy $ DimsExplicitShape $ d :| ds
 where
  scalarTy = typeSpecToScalarType st tySpec
  go (DimensionDeclarator _ _ (Just lb) (Just ub)) dims =
      Dim (constInt lb) (constInt ub) : dims
  go (DimensionDeclarator _ _ Nothing   (Just ub)) dims =
      Dim 1 (constInt ub) : dims
  go _ _ = error "Invalid dimension declarator"
  constInt x = case eval st x of
    Int y -> y
    _     -> error "Invalid array spec"

-- | Given the typespec of a scalar get the StaticType
typeSpecToScalarType :: SymbolTable -> TypeSpec (Analysis a) -> Type
typeSpecToScalarType st (TypeSpec _ _ ty selector) =
  let ty' = baseToType ty
  in  case selector of
        Just (Selector _ _ _        (Just k)) -> setTypeKind ty' (constInt k)
        Just (Selector _ _ (Just l) _       ) -> setTypeKind ty' (constInt l)
        Nothing -> setTypeKind ty' (Just $ kindOfBaseType ty)
        _                                     -> error "Invalid type spec"
 where
  constInt x = case eval' st x of
    Right (Int y) -> Just y
    _             -> Nothing
