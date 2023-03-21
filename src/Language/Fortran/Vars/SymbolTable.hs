{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Vars.SymbolTable
  ( collectSymbols
  )
where

import           Data.Data                      ( Data
                                                , toConstr
                                                )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe, fromMaybe )

import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList
                                                , aStrip
                                                , Block(..)
                                                , CommonGroup(..)
                                                , Declarator(..)
                                                , DeclaratorType(..)
                                                , DimensionDeclarator(..)
                                                , Expression(..)
                                                , Name
                                                , ProgramUnit(..)
                                                , programUnitBody
                                                , Statement(..)
                                                , Selector(..)
                                                , TypeSpec(..)
                                                , Value(..)
                                                )

import           Language.Fortran.Vars.Eval     ( eval
                                                , eval'
                                                )
import           Language.Fortran.Vars.BozConstant
                                                ( resolveBozConstant
                                                , bozToInt
                                                )
import           Language.Fortran.Vars.Types    ( ExpVal(..)
                                                , SymbolTableEntry(..)
                                                , Type
                                                , SemType(..)
                                                , CharacterLen(..)
                                                , SymbolTable
                                                , Dim(..), Dims(..), Dimensions
                                                )
import           Language.Fortran.Vars.Utils    ( typeSpecToScalarType
                                                , typeSpecToArrayType
                                                )
import           Language.Fortran.Vars.Kind     ( getKind
                                                , getTypeKind
                                                , setTypeKind
                                                , getKindOfExpVal
                                                , typeOfExpVal
                                                , baseToType
                                                , isStr
                                                )

-- | Given a 'SymbolTable' and a 'DimensionDeclarator', return a pair of
-- resolved 'DynamicDimensionElement's representing lower- and upper- bound
resolveDimensionDimensionDeclarator
  :: SymbolTable -> DimensionDeclarator (Analysis a) -> Maybe (Int, Int)
resolveDimensionDimensionDeclarator symTable (DimensionDeclarator _ _ lowerbound upperbound)
  = do
    lb <- valueOf lowerbound
    ub <- valueOf upperbound
    pure (lb, ub)
 where
  valueOf (Just (ExpValue _ _ ValStar)) = Nothing
  valueOf (Just (ExpValue _ _ (ValVariable name))) =
    case M.lookup name symTable of
      Just (SParameter _ (Int i)) -> Just i
      _                           -> Nothing
  valueOf (Just expr) = case eval' symTable expr of
    Right (Int i) -> Just i
    _             -> Nothing
  valueOf Nothing = Just 1

-- Parameter declarations
-- A parameter may or may not have a type declaration. If it does have one,
-- the declaration statement can go before or after the parameter statement.
handleParameter
  :: Data a => SymbolTable -> AList Declarator (Analysis a) -> SymbolTable
handleParameter symTable alist = foldl' f symTable (aStrip alist)
 where
  -- special case: immediate BOZ constant
  f symt (Declarator _ _ varExp ScalarDecl _ (Just (ExpValue _ _ (ValBoz boz)))) =
      let symbol = srcName varExp
       in case M.lookup symbol symt of
            Nothing -> symt
            Just (SVariable ty _) -> case ty of
              TInteger kind ->
                let val = bozToInt kind boz
                 in M.insert symbol (SParameter ty val) symt
              _ -> symt -- unhandled BOZ coercion
            Just _ -> symt -- unhandled BOZ usage

  f symt (Declarator _ _ varExp ScalarDecl _ (Just valExp)) =
    let symbol = srcName varExp
        val'   = case eval symt valExp of
          boz@(Boz _) -> resolveBozConstant symTable symbol boz
          v           -> v
        kind' = getKindOfExpVal val'   -- infer kind from value
        pd'   = SParameter (setTypeKind (typeOfExpVal val') (Just kind')) val'
        entry = case M.lookup symbol symt of
                 -- Entry found implies there is a preceding declaration
                 -- of the name. 
                 -- If that is variable declaration, keep the accurate type
                 -- and kind informatio from the declaration.
                 -- Else if it is dummy variable, keep the accurate type 
                 -- and update kind
                 -- Else raise error for conflicting parameter attribute
                 -- Parameter name does not necessarily have a type
                 -- declaration or a kind is assumed. In that case type
                 -- and kind are inferred from the value of parameter.
          Nothing               -> pd'
          Just (SVariable ty _) -> case ty of
            -- TODO previously TCharacter Nothing
            TCharacter CharLenStar _ -> pd'
            _                        -> SParameter ty val'
          Just SDummy{} | isStr val' -> pd'
          Just _ ->
            let errStr t =
                    "Invalid PARAMETER statement for symbol \'" ++ t ++ "\'"
            in  error $ errStr symbol
    in  M.insert symbol entry symt
  f symt _ = symt

handleDeclaration
  :: Data a
  => SymbolTable
  -> TypeSpec (Analysis a)
  -> AList Declarator (Analysis a)
  -> SymbolTable
handleDeclaration symTable typespec decls = foldl' f symTable (aStrip decls)
 where
  (TypeSpec _ _ bt selector) = typespec
  handleVarStar symbol symt ty' =
    let
      entry = case M.lookup symbol symt of
        -- Entry found implies the name also appears in a
        -- preceding parameter statement. In case of ValStar
        -- selector, only type is updated.
        Just (SParameter _ val) -> SParameter ty' val
        Just _                  -> error
          (symbol
          ++ "is not a parameter. \
               \Only ParameterEntries are expected at this point."
          )
        Nothing -> SVariable (TCharacter CharLenStar 1) (symbol, 0)
    in  M.insert symbol entry symt
  -- don't care initial value at this moment
  f symt (Declarator _ s varExp ScalarDecl charLength _) =
    let
      symbol = srcName varExp
      ty'    = baseToType bt
    in
      case (selector, charLength) of
        (Just (Selector _ _ (Just (ExpValue _ _ ValStar)) _), Nothing) ->
          handleVarStar symbol symt ty'
        (_, Just (ExpValue _ _ ValStar)) -> handleVarStar symbol symt ty'
        _ ->
          let
            kind' = getKind symt typespec charLength
            ty''  = setTypeKind ty' kind'
            entry = case M.lookup symbol symt of
              -- Entry found implies the name also appears in a
              -- preceding parameter statement or that the entry
              -- has already been defined. In the case of parameter
              -- only type and kind are updated, and the type and
              -- kind are checked in the case of already defined.
              Just (SParameter _ val) -> SParameter ty'' val
              Just (SVariable (TArray _ dims) loc) ->
                SVariable (TArray ty' dims) loc
              Just v@(SVariable ty loc) ->
                let errStr =
                        "The second declaration of '"
                          ++ symbol
                          ++ "' at line "
                          ++ show s
                          ++ " does not have the same type as the first"
                in  if toConstr ty' /= toConstr ty
                      then error errStr
                      else
                        let mk = getTypeKind ty'
                        in  if mk /= getTypeKind ty
                              then SVariable ty'' loc
                              else v
              Just _ -> error
                (symbol
                ++ " is not a parameter nor array-type variable.\
                                         \ Invalid Fortran syntax at "
                ++ show s
                )
              Nothing -> SVariable ty'' (symbol, 0)
          in
            M.insert symbol entry symt
  f symt (Declarator _ _ varExp (ArrayDecl dimDecls) charLength _) =
    let
      symbol = srcName varExp
      entry  = case charLength of
        Just (ExpValue _ _ ValStar) ->
          let ty = TArray (TCharacter CharLenStar 1) (DimsAssumedSize Nothing 1)
          in  SVariable ty (symbol, 0)
        _ ->
          case resolveDims symt (aStrip dimDecls) of
            Nothing -> error "unsupported dimension declarators: probably skip instead of erroring"
            Just dims ->
              let kd = getKind symt typespec charLength
                  ty = setTypeKind (baseToType bt) kd
              in  SVariable (TArray ty dims) (symbol, 0)
    in
      M.insert symbol entry symt

-- | Handle an array 'Declarator'.
--
-- 'Declarator's are the RHS of a declaration statement, and also used in COMMON
-- block definitions. They store the variable name, and array type info.
-- Importantly, they don't store any scalar info (only bring the variable into
-- scope). So we only handle array 'Declarator's.
--
-- If the array 'Declarator' is for a variable not (yet) in the given
-- 'SymbolTable', it's given a placeholder scalar type. This is apparently
-- inconsistent with how DIMENSION statements are handled, where such cases are
-- skipped.
handleArrayDecl
  :: Data a
  => SymbolTable
  -> Expression (Analysis a)
  -> [DimensionDeclarator (Analysis a)]
  -> SymbolTable
handleArrayDecl symTable varExp dimDecls =
  case resolveDims symTable dimDecls of
    Nothing -> error "unsupported dimension declarators: probably skip instead of erroring"
    Just dims ->
      let symbol = srcName varExp
      in  case M.lookup symbol symTable of
            Just (SVariable TArray{} _) ->
              error "invalid declarator: duplicate array declarations"
            Just (SVariable ty loc) ->
              let ste = SVariable (TArray ty dims) loc
              in  M.insert symbol ste symTable
            Just var -> error $ "Invalid declarator: " <> show var
            Nothing -> -- add array info, use a placeholder for scalar type
              let ste =
                      SVariable (TArray placeholderIntrinsicType dims) (symbol, 0)
              in  M.insert symbol ste symTable
      where placeholderIntrinsicType = TInteger 4

-- | Given a 'SymbolTable' and a 'Statement' found in a 'ProgramUnit', return a new 'SymbolTable'
-- with any newly defined symbols
stSymbols :: Data a => SymbolTable -> Statement (Analysis a) -> SymbolTable
stSymbols symTable = \case
  StParameter _ _ alist        -> handleParameter symTable alist
  StDeclaration _ _ ts _ decls -> handleDeclaration symTable ts decls
  StDimension _ _ decls        -> foldl' handleDimension symTable (aStrip decls)
  StCommon    _ _ cmns         -> foldl' handleCommon symTable (aStrip cmns)
  StInclude _ _ _ (Just bls)   -> foldl' blSymbols symTable bls
  _                            -> symTable
 where
  handleDimension symt = \case
    Declarator _ _ varExp (ArrayDecl dimDecls) _ _ ->
      upgradeScalarToArray (srcName varExp) dimDecls symt
    -- DIMENSION statements only permit array declarators, so this is impossible
    -- in a correct parser.
    Declarator _ _ _ ScalarDecl _ _ ->
      error "non-array declaration in a DIMENSION statement"
  handleCommon symt (CommonGroup _ _ _ decls) =
    let arrayDecls = mapMaybe extractArrayDecl . aStrip $ decls
    in  foldl' (uncurry . handleArrayDecl) symt arrayDecls
  extractArrayDecl = \case
    Declarator _ _ v (ArrayDecl d) _ _ -> Just (v, aStrip d)
    Declarator _ _ _ ScalarDecl    _ _ -> Nothing

-- | Try to upgrade an existing scalar variable to an array variable.
--
-- Returns the unchanged 'SymbolTable' if the variable didn't exist. If the
-- variable was already an array type, runtime error.
--
-- The DIMENSION statement defines array metadata for a variable. Due to
-- Fortran syntax, a variable's the full type isn't known until the executable
-- statements begin, and you may define array and scalar info in either order
-- e.g. `INTEGER x; DIMENSION x(2)` or `DIMENSION x(2); INTEGER x`. This
-- function handles just the former case. (Ideally we handle both
-- interchangeably, but the fortran-vars type representation isn't conducive.)
upgradeScalarToArray
  :: Name
  -> AList DimensionDeclarator (Analysis a)
  -> SymbolTable
  -> SymbolTable
upgradeScalarToArray symbol dimDecls symTable =
  case M.lookup symbol symTable of
    Just (SVariable TArray{} _) ->
      error
        $  symbol
        <> " is array-typed variable."
        <> " Invalid fortran syntax (Duplicate DIMENSION attribute)"
    Just (SVariable ty loc) ->
      let dims = undefined
      -- traverse (resolveDimensionDimensionDeclarator symTable) (aStrip dimDecls)
          entry = SVariable (TArray ty dims) loc
      in  M.insert symbol entry symTable
    _ -> symTable

-- | Given a 'Bool', 'SymbolTable' and a 'ProgramUnit', return an updated
-- 'SymbolTable' containing symbols defined in 'ProgramUnit' signature, e.g.
--   integer function fname() -> symbol table containing 'fname'
-- The first argument flags whether to traverse declarations for the function return
-- type, allowing us to avoid traversing the top level program unit twice
puSymbols
  :: Data a => Bool -> SymbolTable -> ProgramUnit (Analysis a) -> SymbolTable
puSymbols _ symt (PUFunction _ _ (Just typespec) _ symbol _ _ _ _) =
  let entryType = typeSpecToScalarType symt typespec
      entryLoc  = (symbol, 0)
      entry     = SVariable entryType entryLoc
  in  M.insert symbol entry symt
puSymbols getDecls symt (PUFunction _ _ Nothing _ symbol _ _ bls _) =
  if getDecls then foldl' handler symt bls else symt
 where
  handler symt' (BlStatement _ _ _ (StDeclaration _ _ typespec _ decls)) =
    let mty = declToType symt' symbol typespec $ aStrip decls
    in  case mty of
          Just ty ->
            let entryLoc = (symbol, 0)
                entry    = SVariable ty entryLoc
            in  M.insert symbol entry symt'
          Nothing -> symt'
  handler symt' _ = symt'
puSymbols _ symt _ = symt

-- | Given a TypeSpec and list of Declarators, search for a name in that list
-- and return the resolved type if there
declToType
  :: SymbolTable
  -> Name
  -> TypeSpec (Analysis a)
  -> [Declarator (Analysis a)]
  -> Maybe Type
declToType symt name tyspec (d : ds) = if name == getName d
  then Just $ toType d
  else declToType symt name tyspec ds
 where
  getName (Declarator _ _ (ExpValue _ _ (ValVariable str)) _ _ _) = str
  getName _ = error "Unexpected declaration expression"
  toType (Declarator _ _ _ (ArrayDecl dims) _ _) =
    typeSpecToArrayType symt (aStrip dims) tyspec
  toType (Declarator _ _ _ ScalarDecl _ _) = typeSpecToScalarType symt tyspec
declToType _ _ _ [] = Nothing

-- | Update SymbolTable for a given block, traverse statements to get
-- declarations and interfaces to get function signatures.
blSymbols :: Data a => SymbolTable -> Block (Analysis a) -> SymbolTable
blSymbols symt (BlStatement _ _ _ st     ) = stSymbols symt st
blSymbols symt (BlInterface _ _ _ _ pus _) = foldl' (puSymbols True) symt pus
blSymbols symt _                           = symt

-- | Given a 'ProgramUnit', generate a 'SymbolTable' for all of the non-intrisic symbols
collectSymbols :: Data a => ProgramUnit (Analysis a) -> SymbolTable
collectSymbols pu =
  let puSignatureSymbols = puSymbols False M.empty pu
  in  foldl' blSymbols puSignatureSymbols $ programUnitBody pu

-- lower bound always defaults to 1
data Dimension
  = DimensionDynamic (Maybe Int)
  -- ^ dynamic dimension: no upper bound, optional lower bound

  | DimensionStatic  (Maybe Int) Int
  -- ^ static dimension: known upper bound, optional lower bound

coerceDimensionList :: [Dimension] -> Maybe Dimensions
coerceDimensionList = const Nothing -- TODO

-- | Evaluate array dimension information.
--
-- There are essentially three possible return values:
--
--   * static-size array, where all dimensions are fully defined
--   * assumed-size array, where final dimension is dynamic
--   * other (e.g. assumed-shape, unsupported), all information discarded
resolveDims
  :: SymbolTable -> [DimensionDeclarator a] -> Maybe Dimensions
resolveDims symt =
    coerceDimensionList . fromMaybe [] . sequence . map (resolveDim symt)

-- `Nothing` means totally invalid.
resolveDim
  :: SymbolTable -> DimensionDeclarator a -> Maybe Dimension
resolveDim symt (DimensionDeclarator _ _ lb ub) =
    case ub of
      Just (ExpValue _ _ ValStar) -> do
        -- final dimension is @*@: assumed-size
        -- note that we can't represent a lower bound here due to the data
        -- type. oops. TODO
        pure $ DimensionDynamic mlbv
      _ -> do
        case ub of
          Nothing -> Nothing
          Just jub -> do
            ubv <- resolveDimBound symt jub
            pure $ DimensionStatic  mlbv ubv
  where
    mlbv = case lb of Nothing -> Nothing; Just jlb -> resolveDimBound symt jlb

-- note that we don't handle @*@ here. do that manually, for the last dimension
-- (and only for the upper bound -- TODO I think? check standard). if it's not
-- the last dimension, it's not an assumed-size array and so we can throw
-- everything out
resolveDimBound
  :: SymbolTable -> Expression a -> Maybe Int
resolveDimBound symt = \case
  ExpValue _ _ (ValVariable name) ->
    case M.lookup name symt of
      Just (SParameter _ (Int i)) -> Just i
      _                           -> Nothing
  e ->
    case eval' symt e of
      Right (Int i) -> Just i
      _             -> Nothing
