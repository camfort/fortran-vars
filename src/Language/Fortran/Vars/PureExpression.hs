module Language.Fortran.Vars.PureExpression
  ( isPureExpression
  )
where
import           Data.Maybe                     ( catMaybes )

import           Language.Fortran.AST           ( Expression(..)
                                                , Index(..)
                                                , Value(..)
                                                , aStrip
                                                )
import           Language.Fortran.Vars.Call     ( functionArguments )

-- | Given an 'Expression', determine whether it is a pure expression. 
-- A pure expression does not have side effect.
-- return true if the expression is guaranteed to be pure,
-- return false if the expression can not be guaranteed to be pure. 
isPureExpression :: Expression a -> Bool
isPureExpression (ExpValue _ _ v) = True -- All values are pure now
isPureExpression (ExpBinary _ _ _ e1 e2) =
  isPureExpression e1 && isPureExpression e2
isPureExpression (ExpUnary _ _ _ e) = isPureExpression e
isPureExpression (ExpSubscript _ _ _ indices) =
  all isPureIndex (aStrip indices)
isPureExpression e@ExpFunctionCall{} =
  isIntrinsicFunctionCall e && all isPureExpression (functionArguments e)
isPureExpression (ExpInitialisation _ _ exprs) =
  all isPureExpression (aStrip exprs)
isPureExpression ExpReturnSpec{} = False
isPureExpression ExpImpliedDo{}  = False
isPureExpression ExpDataRef{}    = False


-- | Given an 'Index', determine whether it is pure
isPureIndex :: Index a -> Bool
isPureIndex (IxSingle _ _ _ e) = isPureExpression e
isPureIndex (IxRange _ _ me1 me2 me3) =
  all isPureExpression $ catMaybes [me1, me2, me3]

-- | Given an expression determine whether it is intrinsic function call
isIntrinsicFunctionCall :: Expression a -> Bool
isIntrinsicFunctionCall (ExpFunctionCall _ _ (ExpValue _ _ (ValIntrinsic _)) _)
  = True
isIntrinsicFunctionCall _ = False
