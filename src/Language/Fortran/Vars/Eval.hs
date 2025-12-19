{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Vars.Eval
  ( eval
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

import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , ExpVal(..)
                                                , SymbolTable
                                                )

import qualified Language.Fortran.Vars.Eval.FortranSrc as ViaFS
import qualified Language.Fortran.Repr as FS
import qualified Language.Fortran.Repr.Eval.Common as FS.Eval
import qualified Language.Fortran.Repr.Eval.Value as FS.Eval

import Language.Fortran.Analysis (initAnalysis)

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map

-- | Given a 'SymbolTable' and some 'Expression', attempt to evaluate that
--   expression into a value in fortran-src's representation, translate it into
--   an 'ExpVal', and return.
eval' :: SymbolTable -> Expression a -> Either String ExpVal
eval' symt expr =
    case ViaFS.runEval symt (FS.Eval.evalExpr (initAnalysis expr)) of
      Left err -> Left $ show err
      Right a -> ViaFS.translateFValue a

-- | Given a 'SymbolTable' and some 'Expression', evaluate that expression
-- into a basic type and return it as an 'ExpVal'.
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
evalWithShortcircuit = error "TODO unimplemented in fortran-src evaluator"
