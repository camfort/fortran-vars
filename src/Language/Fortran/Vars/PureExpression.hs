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
import           Language.Fortran.AST.Literal.Complex
                                                ( ComplexLit(..)
                                                , ComplexPart(..)
                                                )
import           Language.Fortran.Vars.Call
                                                ( functionArguments )

-- | Given an 'Expression', determine whether it is a pure expression. 
-- A pure expression does not have side effect.
-- return true if the expression is guaranteed to be pure,
-- return false if the expression can not be guaranteed to be pure. 
isPureExpression :: Expression a -> Bool
isPureExpression (ExpValue _ _ v) = isPureValue v
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


-- | Given a 'Value', determine whether it is pure
isPureValue :: Value a -> Bool
isPureValue ValInteger{}       = True
isPureValue ValReal{}          = True
isPureValue (ValComplex c)     = complexLitIsPure c
isPureValue ValString{}        = True
isPureValue ValHollerith{}     = True
isPureValue ValVariable{}      = True
isPureValue ValIntrinsic{}     = True
isPureValue ValLogical{}       = True
isPureValue ValStar            = True
isPureValue _                  = False

-- | Is the given COMPLEX literal "pure", i.e. does it have no named constant
--   components?
complexLitIsPure :: ComplexLit a -> Bool
complexLitIsPure c =
    check (complexLitRealPart c) && check (complexLitImagPart c)
  where check = \case ComplexPartNamed{} -> False
                      _                  -> True

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
