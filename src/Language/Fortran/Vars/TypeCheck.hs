{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fortran.Vars.TypeCheck
  ( Kind
  , TypeError(..)
  , TypeOf
  , typeOf
  )
where
import           Prelude                 hiding ( GT
                                                , EQ
                                                , LT
                                                )
import           Data.List.NonEmpty             ( NonEmpty( (:|) ) )
import qualified Data.Map                      as M
import           Data.Data                      ( toConstr )
import           Data.Maybe                     ( fromJust )
import           Language.Fortran.AST           ( Expression(..)
                                                , Value(..)
                                                , AList(..)
                                                , aStrip
                                                , Argument(..)
                                                , argExprNormalize
                                                , DoSpecification(..)
                                                , Statement(..)
                                                , Name
                                                , BinaryOp(..)
                                                , Index(..)
                                                )
import           Language.Fortran.AST.Literal   ( KindParam(..) )
import           Language.Fortran.AST.Literal.Real
                                                ( RealLit(..)
                                                , Exponent(..)
                                                , ExponentLetter(..)
                                                )
import           Language.Fortran.AST.Literal.Complex
                                                ( ComplexLit(..)
                                                , ComplexPart(..)
                                                )
import           Language.Fortran.Intrinsics    ( getVersionIntrinsics
                                                , getIntrinsicReturnType
                                                , IntrinsicType(..)
                                                )
import           Language.Fortran.Version       ( FortranVersion(..) )

import           Language.Fortran.Util.Position ( SrcSpan
                                                , getSpan
                                                )
import           Language.Fortran.Vars.Types    ( SymbolTableEntry(..)
                                                , ExpVal(..)
                                                , SymbolTable
                                                , StructureTable
                                                , Kind
                                                , Type
                                                , SemType(..)
                                                , CharacterLen(..)
                                                , TypeError(..)
                                                , TypeOf
                                                , typeError
                                                , Dim(..)
                                                , Dims(..)
                                                )
import           Language.Fortran.Vars.Kind     ( getTypeKind
                                                , setTypeKind
                                                , toInt
                                                )
import           Language.Fortran.Vars.Eval     ( eval' )
import           Language.Fortran.Vars.StructureTable
                                                ( lookupField )

import           Language.Fortran.Analysis.SemanticTypes
                                                ( charLenConcat )


-- | Given 'SymbolTable' of a 'ProgramUnit', and an 'Expression' within
-- the 'ProgramUnit', determines the 'Type' of the 'Exression'
typeOf :: StructureTable -> SymbolTable -> Expression a -> Either TypeError Type
typeOf strTable symTable expr = case expr of
  ExpValue _ _ (ValVariable name) -> typeOfSymbol symTable name
  ExpValue _ s val                -> typeOfValue s strTable symTable val
  ExpUnary _ _ _ e                -> typeOf strTable symTable e
  ExpBinary _ s op e1 e2 -> typeOfBinaryExp s strTable symTable op e1 e2
  ExpFunctionCall _ _ (ExpValue _ s (ValVariable name)) args ->
    typeOfFunctionCall s strTable symTable name (aStrip args)
  ExpFunctionCall _ _ (ExpValue _ s (ValIntrinsic name)) args ->
    typeOfFunctionCall s strTable symTable name (aStrip args)
  ExpSubscript _ s arr (AList _ _ args@(IxSingle{} : _)) ->
    let isIxRange = \case
          IxRange{} -> True
          _         -> False
    in  if any isIxRange args
          then Left . typeError s $ "Unexpected array range"
          else case typeOf strTable symTable arr of
            Right (TArray ty _) -> Right ty
            Right _ ->
              Left . typeError s $ "Tried to access elements of scalar"
            err -> err
  ExpSubscript _ s arr (AList _ _ (r@IxRange{} : _)) -> do
    ty <- typeOf strTable symTable arr
    case ty of
      TCharacter _ _ -> typeOfSubString s symTable strTable ty r
      _              -> Right ty
  ExpImpliedDo _ _ es doSpec -> do
    dim <- specToDim symTable doSpec
    ty  <- typeOf strTable symTable . head $ aStrip es
    pure $ case ty of
      TArray ty' (DimsExplicitShape (Dim (Just 1) (Just dim') :| [])) ->
        TArray ty' $ DimsExplicitShape $ Dim (Just 1) (Just (dim * dim')) :| []
      TArray _ _ -> error "Unexpected array type in implied do"
      _ ->
        TArray ty  $ DimsExplicitShape $ Dim (Just 1) (Just dim) :| []

  ExpDataRef _ _ es (ExpValue _ _ (ValVariable name)) -> do
    ty <- typeOf strTable symTable es
    lookupField strTable ty name
  _ -> Left . UnknownType $ getSpan expr

-- | Internal function to get array size out of a DoSpecification
specToDim :: SymbolTable -> DoSpecification a -> Either TypeError Int
specToDim symt (DoSpecification _ s (StExpressionAssign _ _ _ start) end step)
  = let evalInt x = case eval' symt x of
          Right (Int y) -> Right y
          Right _ -> Left . typeError s $ "non int value in do spec " <> show s
          Left err -> Left . typeError s $ err
    in  do
          start' <- evalInt start
          end'   <- evalInt end
          case step of
            Just x -> do
              step' <- evalInt x
              pure $ ((end' - start') `div` step') + 1
            Nothing -> pure $ (end' - start') + 1
specToDim _ _ = error "Unexpected do specification structure"

-- | Internal function to determine the 'Type' of a symbol
typeOfSymbol :: SymbolTable -> Name -> Either TypeError Type
typeOfSymbol symTable name = case M.lookup name symTable of
  Just entry -> case entry of
    SParameter t _ -> Right t
    SVariable  t _ -> Right t
    SDummy    t    -> Right t
    SExternal t    -> Right t
  Nothing -> Left $ UnboundVariable name

-- | Internal function to determine the 'Type' of a constant
--
-- TODO ignoring kind param errors (should report better)
typeOfValue
  :: SrcSpan
  -> StructureTable
  -> SymbolTable
  -> Value a
  -> Either TypeError Type
typeOfValue sp strTable symTable v = case v of
  ValInteger _ mkp -> Right $ TInteger (kpOrDef 4 mkp)
  ValReal r _ -> -- TODO ignoring kind param
    let k = case exponentLetter (realLitExponent r) of
          ExpLetterE -> 4
          ExpLetterD -> 8
          ExpLetterQ -> 16
    in  Right $ TReal k
  ValComplex c -> do
    tr <- typeOfComplexPart strTable symTable $ complexLitRealPart c
    ti <- typeOfComplexPart strTable symTable $ complexLitImagPart c
    if tr == TReal 8 || ti == TReal 8
      then return (TComplex 16)
      else return (TComplex 8)
  ValString    s   -> Right $ TCharacter (CharLenInt (length s)) 1
  ValHollerith s   -> Right . TByte $ length s
  ValLogical _ mkp -> Right $ TLogical (kpOrDef 4 mkp)
  ValBoz _         -> Right $ TByte 4
  _                -> Left $ UnknownType sp
  where
    kpOrDef :: Kind -> Maybe (KindParam a) -> Kind
    kpOrDef kDef = \case
      Nothing -> kDef
      Just kp -> case kp of
        KindParamInt _ _ kpLit -> read kpLit
        KindParamVar _ _ kpVar ->
          let kpVarExpr = ExpValue undefined undefined (ValVariable kpVar)
          in  case eval' symTable kpVarExpr of
                Left{} -> kDef
                Right k -> toInt k

promote :: Type -> Type -> Type
promote t1 t2
  | toConstr t1 == toConstr t2 = case
      max <$> getTypeKind t1 <*> getTypeKind t2
    of
      Just k -> setTypeKind t1 $ Just k
      Nothing ->
        error
          $  "dynamic type in promotion between: "
          <> show t1
          <> " and "
          <> show t2
  | otherwise = case (t1, t2) of
    (TComplex k , _          ) -> TComplex k
    (_          , TComplex k ) -> TComplex k
    (TReal k    , _          ) -> TReal k
    (_          , TReal k    ) -> TReal k
    (TLogical k1, TInteger k2) -> TInteger $ max k1 k2
    (TInteger k1, TLogical k2) -> TInteger $ max k1 k2
    (TInteger k , _          ) -> TInteger k
    (_          , TInteger k ) -> TInteger k
    (TLogical k , _          ) -> TLogical k
    (_          , TLogical k ) -> TLogical k
    _ -> error $ "Incompatible types: " <> show t1 <> " " <> show t2

-- | Internal function fo determine type of binary expression
typeOfBinaryExp
  :: SrcSpan
  -> StructureTable
  -> SymbolTable
  -> BinaryOp
  -> Expression a
  -> Expression a
  -> Either TypeError Type
typeOfBinaryExp sp strTable symTable op e1 e2
  |
  -- Relational
    op `elem` [GT, GTE, LT, LTE, EQ, NE] = Right (TLogical 4)
  | otherwise = do
    st1 <- typeOf strTable symTable e1
    st2 <- typeOf strTable symTable e2
    typeOfBinaryExp' sp op st1 st2

-- | Internal funciton for determining type of binary expression.
typeOfBinaryExp' :: SrcSpan -> BinaryOp -> Type -> Type -> Either TypeError Type
typeOfBinaryExp' sp op t1 t2
  |
  -- Character
    op == Concatenation
  -- TODO
  -- = Right . TCharacter $ (+) <$> k1 <*> k2
  = case t1 of
    TCharacter l1 k1' -> case t2 of
      TCharacter l2 _ -> Right $ TCharacter (charLenConcat l1 l2) k1'
      _               -> error "shit 1"
    _ -> error "shit 2"
  |
  -- Logical
  -- NB when integer's are used with logical operators you get bitwise
  -- arithmetic behaviour
    op `elem` [And, Or, Equivalent, NotEquivalent, XOr]
  = let
      ty = case (t1, t2) of
        (TLogical _, TLogical _) -> Right . TLogical
        (TInteger _, _         ) -> Right . TInteger
        (_         , TInteger _) -> Right . TInteger
        (TByte _   , _         ) -> Right . TInteger
        (_         , TByte _   ) -> Right . TInteger
        _                        -> const
          (Left $ typeError sp "Unexpected types used with logical operators")
    in  ty . fromJust $ max <$> k1 <*> k2
  |
  -- Arithmetic
    op `elem` [Addition, Subtraction, Multiplication, Division, Exponentiation]
  = Right $ promote t1 t2
  | otherwise
  = Left $ UnknownType sp
 where
  k1 = getTypeKind t1
  k2 = getTypeKind t2


-- | Internal function to determine the type of a substring
-- If either of the indexes cannot be evaluated then we return a dynamically
-- sized character type
-- TODO this is the worst one
typeOfSubString
  :: SrcSpan
  -> SymbolTable
  -> StructureTable
  -> Type
  -> Index a
  -> Either TypeError Type
typeOfSubString sp symt strt ty (IxRange _ _ lower upper _) = do
  isInteger $ traverse (typeOf strt symt) lower
  isInteger $ traverse (typeOf strt symt) upper
  pure $ TCharacter calcLen 1
 where
  calcLen = case (\x y -> y - x + 1) <$> lowerIndex <*> upperIndex of
    Nothing  -> CharLenStar
    Just len -> CharLenInt len
  isInteger = \case
    Right (Just (TInteger _)) -> Right ()
    Right Nothing -> Right ()
    _ -> Left . typeError sp $ "Index  wasn't an integer type"
  upperIndex = let Just k = getTypeKind ty in getIndex k upper
  lowerIndex = getIndex 1 lower
  getIndex :: Int -> Maybe (Expression a) -> Maybe Int
  getIndex dflt Nothing  = Just dflt
  getIndex _    (Just e) = case eval' symt e of
    Right (Int i) -> Just i
    _             -> Nothing

typeOfSubString _ _ _ _ idx = Left $ UnknownType (getSpan idx)

-- | determine the return type of a function call
typeOfFunctionCall
  :: SrcSpan
  -> StructureTable
  -> SymbolTable
  -> Name
  -> [Argument a]
  -> Either TypeError Type
typeOfFunctionCall sp strT symT name argList =
  checkIntrinsicFunction <> checkF77IntrinsicFunction <> checkExternalFunction
 where
  args = [ argExprNormalize e | Argument _ _ _ e <- argList ]
  -- If the function is any of the intrinsics below, determine its return type
  -- accordingly
  checkIntrinsicFunction :: Either TypeError Type
  checkIntrinsicFunction
    | name `elem` ["int", "nint"], length args == 1
    = Right (TInteger 4)
    | name `elem` ["int", "nint"], length args == 2
    = case eval' symT (args !! 1) of
      Right (Int k) -> Right (TInteger k)
      _             -> Left $ typeError
        sp
        (  "Unable to determine the second argument value of "
        <> name
        <> " function"
        )
    | name == "int2"
    = Right (TInteger 2)
    | name `elem` ["loc", "sizeof", "iachar"]
    = Right (TInteger 4)
    | name == "dfloat"
    = Right (TReal 8)
    | name `elem` ["ishft", "lshift", "rshift", "ibset", "ibits"], not
      (null args)
    = typeOf strT symT (head args)
    | name `elem` ["iand", "ior", "ieor", "and"], length args == 2
    = do
      t1 <- typeOf strT symT (head args)
      t2 <- typeOf strT symT (args !! 1)
      return $ promote t1 t2
    | name == "imag", length args == 1
    = do
      ty <- typeOf strT symT (head args)
      case ty of
        TComplex x -> Right . TReal $ x `div` 2
        _          -> Left $ typeError sp "Invalid argument to imag"
    | name == "btest", length args == 2
    = return $ TLogical 4
    | name == "not", length args == 1
    = typeOf strT symT (head args)
    | otherwise
    = Left $ typeError
      sp
      (name <> " is not in the extra list of intrinsic functions")

  -- Otherwise, if the function is listed in fortran-src's Fortran77 intrinsic
  -- table, get return type from the intrinsic table.
  checkF77IntrinsicFunction :: Either TypeError Type
  checkF77IntrinsicFunction =
    let f77intrinsics = getVersionIntrinsics Fortran77
    in
      case getIntrinsicReturnType name f77intrinsics of
        Just ITReal      -> Right (TReal 4)
        Just ITInteger   -> Right (TInteger 4)
        Just ITComplex   -> Right (TComplex 8)
        Just ITDouble    -> Right (TReal 8)
        Just ITLogical   -> Right (TLogical 4)
        Just ITCharacter -> Right (TCharacter (CharLenInt 1) 1)
        Just (ITParam i)
          | length args >= i -> typeOf strT symT (args !! (i - 1))
          | otherwise -> Left $ typeError
            sp
            ("Wrong number of arguments for intrinsic function " <> name)
        Nothing ->
          Left $ typeError sp (name <> " is not in Fortran 77 intrinsic table")

  -- If the function is an external function, its type should have been captured
  -- in the symbol table.
  checkExternalFunction :: Either TypeError Type
  checkExternalFunction = typeOfSymbol symT name

typeOfComplexPart :: StructureTable -> SymbolTable -> ComplexPart a -> Either TypeError Type
typeOfComplexPart strTable symTable = \case
  ComplexPartReal   _ ss cpReal mkp -> tOfVal ss (ValReal    cpReal mkp)
  ComplexPartInt    _ ss cpInt  mkp -> tOfVal ss (ValInteger cpInt  mkp)
  ComplexPartNamed  _ _ nm         -> typeOfSymbol symTable nm
  where tOfVal ss v = typeOfValue ss strTable symTable v
