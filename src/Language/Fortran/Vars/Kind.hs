{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Vars.Kind
  ( module Language.Fortran.Vars.Kind
  , kindOfBaseType
  , getTypeSize
  , setTypeSize
  , deriveSemTypeFromBaseType
  )
where

import           Data.Maybe                     ( fromJust )
import           Language.Fortran.Analysis      ( Analysis )
import           Language.Fortran.Analysis.Types
                                                ( deriveSemTypeFromBaseType )
import           Language.Fortran.AST           ( BaseType(..)
                                                , Expression(..)
                                                , Selector(..)
                                                , TypeSpec(..)
                                                , Value(..)
                                                )
import           Language.Fortran.Analysis.SemanticTypes
                                                ( kindOfBaseType
                                                , getTypeSize
                                                , setTypeSize
                                                )

import           Language.Fortran.Vars.Errors   ( invalidArg )
import           Language.Fortran.Vars.Eval     ( eval' )
import           Language.Fortran.Vars.Types    ( ExpVal(..)
                                                , Type
                                                , SemType(..)
                                                , CharacterLen(..)
                                                , SymbolTable
                                                )

baseToType :: BaseType -> Type
baseToType = deriveSemTypeFromBaseType

getTypeKind :: Type -> Maybe Int
getTypeKind = getTypeSize

setTypeKind :: Type -> Maybe Int -> Type
setTypeKind = setTypeSize

-- | Given an 'ExpVal', return the 'Type' of that value
typeOfExpVal :: ExpVal -> Type
typeOfExpVal = \case
  Int     _ -> TInteger 4
  Real    _ -> TReal 4
  Logical _ -> TLogical 4
  Str     _ -> TCharacter (CharLenInt 1) 1
  Boz     _ -> error "BOZ constant is type-less"

-- | Given an 'ExpVal', return true if it is a 'Str', else false
isStr :: ExpVal -> Bool
isStr e = case e of
  Str _ -> True
  _     -> False

-- | Given an 'ExpVal' determine the kind of it
getKindOfExpVal :: ExpVal -> Int
getKindOfExpVal (Str s) = length s
getKindOfExpVal ev      = fromJust . getTypeSize . typeOfExpVal $ ev

-- | Convert an 'ExpVal' to an 'Int'. This will fail if the
-- 'ExpVal' is not already known to be an 'Int'
toInt :: ExpVal -> Int
toInt (Int i) = i
toInt e       = invalidArg "toInt" [e]

-- | Given a 'SymbolTable', a 'TypeSpec' for a variable, and possibly an 'Expression'
-- for the length of a character array, determine the kind of that variable
-- The charLength also works for nonstandard kind
getKind
  :: SymbolTable
  -> TypeSpec (Analysis a)
  -> Maybe (Expression (Analysis a))
  -> Maybe Int
getKind symTable (TypeSpec _ _ bt selector) charLength =
  let evalMaybeKind kind =
          either (const Nothing) (Just . toInt) $ eval' symTable kind
  in  case charLength of
        Just charLen -> evalMaybeKind charLen
        Nothing      -> case selector of
          Just (Selector _ _ _ (Just kindExp)) ->
            let k' = kindExp
            in  case k' of
                  ExpValue _ _ ValStar -> Nothing
                  _                    -> evalMaybeKind kindExp
          Just (Selector _ _ (Just lengthExp) _) ->
            let l' = lengthExp
            in  case l' of
                  ExpValue _ _ ValStar -> Nothing
                  _                    -> evalMaybeKind lengthExp
          _ -> Just $ kindOfBaseType bt
