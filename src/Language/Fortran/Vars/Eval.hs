module Language.Fortran.Vars.Eval
  ( eval
  , eval'Orig
  , eval'ViaFS
  , eval'
  , evalWithShortcircuit
  )
where

import           Prelude                 hiding ( fail )
import           Language.Fortran.AST           ( BinaryOp(..)
                                                , Expression(..)
                                                , Value(..)
                                                , AList(..)
                                                , argExtractExpr
                                                )
import           Language.Fortran.Util.Position ( getSpan )

import           Language.Fortran.Vars.Operation
                                                ( binaryOp'
                                                , binaryTransformEither
                                                , nonLogicalToLogical
                                                , transformEither
                                                , transformEitherList
                                                , unaryOp'
                                                , valueToExpVal'
                                                , intrinsicFunctionCall
                                                )
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , ExpVal(..)
                                                , SymbolTable
                                                )

import Language.Fortran.Vars.FSRepr.Translate
import qualified Language.Fortran.Repr as FS
import qualified Language.Fortran.Repr.Eval.Common as FSEval
import qualified Language.Fortran.Repr.Eval.Value as FSEval

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map

eval' :: SymbolTable -> Expression a -> Either String ExpVal
eval' = eval'ViaFS

-- | Given a 'SymbolTable' and some 'Expression', evaluate that expression
-- into a basic type and return it as an 'ExpVal' or a 'String' describing
-- the issue that prevented the evaluation
eval'Orig :: SymbolTable -> Expression a -> Either String ExpVal
eval'Orig symTable expr = case expr of
  ExpValue _ _ (ValVariable name) -> case Map.lookup name symTable of
    Just (SParameter _ expVal) -> Right expVal
    Just _ -> Left $ "Cannot be evaluated: " ++ name ++ " is not a parameter."
    Nothing -> Left $ "Cannot find parameter : " ++ name

  ExpValue _ s val  -> valueToExpVal' s val

  ExpUnary _ _ op e -> transformEither (unaryOp' op) $ eval'Orig symTable e
  ExpBinary _ _ op e1 e2 ->
    binaryTransformEither (binaryOp' op) (eval'Orig symTable e1) (eval'Orig symTable e2)
  ExpFunctionCall _ _ (ExpValue _ _ function) (AList _ _ args) ->
    transformEitherList intrinsicFunctionCall'
      $   eval'Orig symTable
      .   argExtractExpr
      <$> args
   where
    intrinsicFunctionCall' = intrinsicFunctionCall $ functionName function
    functionName (ValVariable  name) = name
    functionName (ValIntrinsic name) = name
    functionName _                   = ""
  _ -> Left $ "Unsupported expression at: " ++ show (getSpan expr)

-- | Given a 'SymbolTable' and some 'Expression', attempt to evaluate that
--   expression into a value in fortran-src's representation, translate it into
--   an 'ExpVal', and return.
eval'ViaFS :: SymbolTable -> Expression a -> Either String ExpVal
eval'ViaFS symt expr = flip runReader symt $ runExceptT $ do
    x <- withExceptT show $ FSEval.evalExpr expr
    ExceptT $ return $ translateFValue x

--newtype Eval = Eval { unEval :: ExceptT Error (Reader SymbolTable) }

-- | Evaluate Fortran values in SymbolTable using the fortran-src
--   representation, implicitly translating between the representations.
instance FSEval.MonadEval (ExceptT FSEval.Error (Reader SymbolTable)) where
    type EvalTo (ExceptT FSEval.Error (Reader SymbolTable)) = FS.FValue
    lookupFVar name = do
        symt <- ask
        case Map.lookup name symt of
          Nothing -> return Nothing
          Just (SParameter _ val) ->
            return $ Just $ FS.MkFScalarValue $ translateExpVal val
          _ -> error "TODO"

    -- | Ignore warnings. fortran-vars doesn't have a method to report warnings
    --   during evaluation.
    warn _ = pure ()

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
  _ -> eval'Orig symTable expr
