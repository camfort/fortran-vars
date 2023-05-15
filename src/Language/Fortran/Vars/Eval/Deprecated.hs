{- | TODO -}

module Language.Fortran.Vars.Eval.Deprecated where

import Language.Fortran.Vars.Eval.Deprecated.Operation

import Language.Fortran.Vars.Rep
import Language.Fortran.Vars.Types.SymbolTable

import Language.Fortran.AST
import Language.Fortran.Util.Position ( getSpan )

import qualified Data.Map as Map

-- | Given a 'SymbolTable' and some 'Expression', evaluate that expression
-- into a basic type and return it as an 'ExpVal' or a 'String' describing
-- the issue that prevented the evaluation
eval' :: SymbolTable -> Expression a -> Either String ExpVal
eval' symTable = \case
  ExpValue _ _ (ValVariable name) -> case Map.lookup name symTable of
    Just (SParameter _ expVal) -> Right expVal
    Just _ -> Left $ "Cannot be evaluated: " ++ name ++ " is not a parameter."
    Nothing -> Left $ "Cannot find parameter : " ++ name

  ExpValue _ s val  -> valueToExpVal' s val

  ExpUnary _ _ op e -> transformEither (unaryOp' op) $ eval' symTable e
  ExpBinary _ _ op e1 e2 ->
    binaryTransformEither (binaryOp' op) (eval' symTable e1) (eval' symTable e2)
  ExpFunctionCall _ _ (ExpValue _ _ function) (AList _ _ args) ->
    transformEitherList intrinsicFunctionCall'
      $   eval' symTable
      .   argExtractExpr
      <$> args
   where
    intrinsicFunctionCall' = intrinsicFunctionCall $ functionName function
    functionName (ValVariable  name) = name
    functionName (ValIntrinsic name) = name
    functionName _                   = ""
  e -> Left $ "Unsupported expression at: " ++ show (getSpan e)

-- | Given a 'SymbolTable' and some 'Expression', evaluate that expression
-- into a basic type and return it as an 'ExpVal'
eval :: SymbolTable -> Expression a -> ExpVal
eval symTable expr = case eval' symTable expr of
  Left  err -> error (err ++ show (getSpan expr))
  Right r   -> r

-- | Given a 'SymbolTable' and some 'Expression', evaluate that expression
-- into a basic type and return it as an 'ExpVal' or a 'String' describing
-- the issue that prevented the evaluation. In the case of expressions like
--
-- @
--       foobar .AND. .FALSE.
--       .TRUE. .OR. .foobar
-- @
--
-- the expressions will be shortcircuited to produce
--
-- @
--       .FALSE.
--       .TRUE.
-- @
evalWithShortcircuit :: SymbolTable -> Expression a -> Either String ExpVal
evalWithShortcircuit symTable expr = case expr of
  ExpUnary _ _ op e ->
    transformEither (unaryOp' op) $ evalWithShortcircuit symTable e
  ExpBinary _ _ op e1 e2 ->
    let e1' = evalWithShortcircuit symTable e1
        e2' = evalWithShortcircuit symTable e2
        t   = transformEither nonLogicalToLogical
    in  case (op, t e1', t e2') of
          (And, Right r    , Right l    ) -> Right . Logical $ r && l
          (And, Right False, _          ) -> Right $ Logical False
          (And, _          , Right False) -> Right $ Logical False
          (Or , Right r    , Right l    ) -> Right . Logical $ r || l
          (Or , Right True , _          ) -> Right $ Logical True
          (Or , _          , Right True ) -> Right $ Logical True
          _ -> binaryTransformEither (binaryOp' op) e1' e2'
  _ -> eval' symTable expr
