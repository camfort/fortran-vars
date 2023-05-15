{- | fortran-vars-style expression evaluation which piggybacks off the evaluator
     in fortran-src.
-}

{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Vars.Eval.FortranSrc
  ( module Language.Fortran.Vars.Eval.FortranSrc
  , module Language.Fortran.Vars.Eval.FortranSrc.Translate
  ) where

import Language.Fortran.Vars.Eval.FortranSrc.Translate

import Language.Fortran.Vars.Types.SymbolTable

import qualified Language.Fortran.Repr as FS.Rep
import qualified Language.Fortran.Repr.Eval.Common as FS.Eval
import qualified Language.Fortran.Repr.Eval.Value as FS.Eval

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map

-- | Fortran expression evaluation monad, using 'SymbolTable' and reporting
--   fortran-src evaluator errors.
--
-- We use a newtype wrapper on this at 'Eval'. The type synonym assists some
-- boilerplate.
type Eval' = ExceptT FS.Eval.Error (Reader SymbolTable)

-- | Fortran expression evaluation monad, using 'SymbolTable' and reporting
--   fortran-src evaluator errors.
newtype Eval a = Eval { unEval :: Eval' a }
    deriving (Functor, Applicative, Monad) via Eval'
    deriving (MonadReader SymbolTable) via Eval'
    deriving (MonadError FS.Eval.Error) via Eval'

-- | Execute a program in the Fortran expression evaluation monad 'Eval'.
runEval :: SymbolTable -> Eval a -> Either FS.Eval.Error a
runEval symt = flip runReader symt . runExceptT . unEval

-- | Evaluate Fortran expressions to 'FS.FValue's.
--
-- We look up variables from a plain 'SymbolTable', but evaluate using
-- fortran-src's machinery. We must therefore translate 'SymbolTable' 'ExpVal's
-- to 'FS.FValue'. If we want to return fortran-vars-style types, we must
-- translate the other way after executing a program in this monad.
instance FS.Eval.MonadFEval Eval where
    type EvalTo Eval = FS.Rep.FValue

    lookupFVar name = do
        symt <- ask
        case Map.lookup name symt of
          Nothing -> return Nothing
          Just entry ->
            case entry of
              SParameter _ val ->
                return $ Just $ FS.Rep.MkFScalarValue $ translateExpVal val
              _ -> do
                FS.Eval.warn $
                    "found variable in SymbolTable, but wasn't an SParameter: "
                    <>name
                return Nothing

    -- | Ignore warnings. fortran-vars doesn't have a method to report warnings
    --   during evaluation.
    warn _ = pure ()
