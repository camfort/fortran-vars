module Language.Fortran.Vars.Call
  ( functionCalls
  , subroutineCalls
  , functionArguments
  , subroutineArguments
  )
where
import           Data.Generics.Uniplate.Data    ( universeBi )
import           Data.Data                      ( Data )
import           Data.Char                      ( toUpper )

import           Language.Fortran.AST           ( Statement(..)
                                                , Expression(..)
                                                , Argument(..)
                                                , aStrip
                                                )
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.Util.Position ( getSpan )

-- | Utility to get all call expressions of the specified function
functionCalls
  :: (Data a, Data (b (Analysis a)))
  => b (Analysis a)
  -> String
  -> [Expression (Analysis a)]
functionCalls x funcName =
  [ e
  | e@(ExpFunctionCall _ _ v _) <- universeBi x
  , caseInsensitiveEqual (srcName v) funcName
  ]

-- | Utility to get all call statements of the specified subroutine
subroutineCalls
  :: (Data a, Data (b (Analysis a)))
  => b (Analysis a)
  -> String
  -> [Statement (Analysis a)]
subroutineCalls x subName =
  [ e | e@(StCall _ _ v _) <- universeBi x, srcName v == subName ]

-- | Given a function call 'Expression', return the list of argument 'Expression'
functionArguments :: Expression a -> [Expression a]
functionArguments (ExpFunctionCall _ _ _ args) = case args of
  Just args' -> map (\(Argument _ _ _ e) -> e) (aStrip args')
  Nothing    -> []
functionArguments e =
  error $ "Expression at " ++ show (getSpan e) ++ " is not a function call"


-- | Given a subroutine call 'Statement', return the list of argument 'Expression'
subroutineArguments :: Statement a -> [Expression a]
subroutineArguments (StCall _ _ _ args) = case args of
  Just args' -> map (\(Argument _ _ _ e) -> e) (aStrip args')
  Nothing    -> []
subroutineArguments s =
  error $ "Statement at " ++ show (getSpan s) ++ " is not a subroutine call"

-- | case-insenstive string equality
caseInsensitiveEqual :: String -> String -> Bool
caseInsensitiveEqual s1 s2 = map toUpper s1 == map toUpper s2
