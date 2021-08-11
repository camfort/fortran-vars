module Language.Fortran.Vars.SymbolTable
  ( collectSymbols
  )
where

import           Data.Data                      ( Data
                                                , toConstr
                                                )
import           Data.List                      ( foldl' )
import qualified Data.Map                      as M
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList
                                                , Argument(..)
                                                , aStrip
                                                , BaseType(..)
                                                , Block(..)
                                                , CommonGroup(..)
                                                , Declarator(..)
                                                , DimensionDeclarator(..)
                                                , Expression(..)
                                                , Index(..)
                                                , Name
                                                , ProgramUnit(..)
                                                , programUnitBody
                                                , Statement(..)
                                                , Selector(..)
                                                , TypeSpec(..)
                                                , Value(..)
                                                )

import           Language.Fortran.Vars.Eval
                                                ( eval
                                                , eval'
                                                )
import           Language.Fortran.Vars.BozConstant
                                                ( resolveBozConstant )
import           Language.Fortran.Vars.Types
                                                ( ExpVal(..)
                                                , SymbolTableEntry(..)
                                                , Type(..)
                                                , SemType(..)
                                                , CharacterLen(..)
                                                , SymbolTable
                                                )
import           Language.Fortran.Vars.Utils
                                                ( typeSpecToScalarType
                                                , typeSpecToArrayType
                                                )
import           Language.Fortran.Vars.Kind
                                                ( getKind
                                                , getTypeKind
                                                , setTypeKind
                                                , getKindOfExpVal
                                                , toInt
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

-- | Given a 'SymbolTable' and an 'Index', return a pair of
-- resolved 'DynamicDimensionElement's representing lower- and upper- bound
resolveDimensionExpSubscript
  :: SymbolTable -> Index (Analysis a) -> Maybe (Int, Int)
resolveDimensionExpSubscript symTable index = case index of
  IxSingle _ _ _ upperbound -> do
    ub <- valueOf upperbound
    pure (1, ub)
  IxRange _ _ lowerbound upperbound _ -> do
    lb <- lowerbound >>= valueOf
    ub <- upperbound >>= valueOf
    pure (lb, ub)
 where
  valueOf expr = case eval' symTable expr of
    Right (Int i) -> Just i
    _             -> Nothing

-- | Given a 'SymbolTable' and an 'Argument', return a maybe pair of Ints
-- representing lower- and upper- bound
resolveDimensionExpFunctionCall
  :: SymbolTable -> Argument (Analysis a) -> Maybe (Int, Int)
resolveDimensionExpFunctionCall symTable (Argument _ _ _ upperbound) =
  let ub = toInt (eval symTable upperbound) in Just (1, ub)

-- Parameter declarations
-- A parameter may or may not have a type declaration. If it does have one,
-- the declaration statement can go before or after the parameter statement.
handleParameter
  :: Data a => SymbolTable -> AList Declarator (Analysis a) -> SymbolTable
handleParameter symTable alist = foldl' f symTable (aStrip alist)
 where
  f symt (DeclVariable _ _ varExp _ (Just valExp)) =
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
  f symt (DeclVariable _ s varExp charLength _) =
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
  f symt (DeclArray _ _ varExp dimDecls charLength _) =
    let
      symbol = srcName varExp
      entry  = case charLength of
        Just (ExpValue _ _ ValStar) ->
          SVariable (TArray (TCharacter CharLenStar 1) Nothing) (symbol, 0)
        _ ->
          let
            kd   = getKind symt typespec charLength
            dims = traverse (resolveDimensionDimensionDeclarator symt)
                            (aStrip dimDecls)
            ty = setTypeKind (baseToType bt) kd
          in
            SVariable (TArray ty dims) (symbol, 0)
    in
      M.insert symbol entry symt

updateDimensionDimensionDeclarator
  :: Name
  -> AList DimensionDeclarator (Analysis a)
  -> SymbolTable
  -> SymbolTable
updateDimensionDimensionDeclarator symbol dimDecls symTable =
  case M.lookup symbol symTable of
    Just (SVariable TArray{} _) -> error
      (symbol
      ++ "is array-typed Varible. \
                 \Invalid fortran syntax (Duplicate DIMENSION attribute)"
      )
    Just (SVariable ty loc) ->
      let mdims = traverse (resolveDimensionDimensionDeclarator symTable)
                           (aStrip dimDecls)
          entry = SVariable (TArray ty mdims) loc
      in  M.insert symbol entry symTable
    _ -> symTable

handleDimension
  :: Data a => SymbolTable -> AList Declarator (Analysis a) -> SymbolTable
handleDimension symTable decls = foldl' f symTable (aStrip decls)
 where
  f symt (DeclArray _ _ varExp dimDecls _ _) =
    updateDimensionDimensionDeclarator (srcName varExp) dimDecls symt
  f symt _ = symt

-- | Given symbol, list of Arguments and SymbolTable, it updates the relevant
-- variable in the SymbolTable to become an array with dimensions described by
-- the list of Arguments.
--
-- This function is needed to handle dimensions specifications within COMMONs,
-- because fortran-src doesn't support 'DimensionDeclarator's within COMMON blocks.
updateDimensionExpFunctionCall
  :: Name -> AList Argument (Analysis a) -> SymbolTable -> SymbolTable
updateDimensionExpFunctionCall symbol args symTable =
  case M.lookup symbol symTable of
    Just (SVariable TArray{} _) -> error
      (symbol
      ++ " is array-typed VaribleEntry. \
                 \Invalid fortran syntax (Duplicate DIMENSION attribute)"
      )
    Just (SVariable std loc) ->
      let dims =
              traverse (resolveDimensionExpFunctionCall symTable) (aStrip args)
          entry = SVariable (TArray std dims) loc
      in  M.insert symbol entry symTable
    Just (SDummy _) -> error
      (symbol
      ++ " is DummyVariableEntry. \
                 \Invalid fortran syntax (Dummy in COMMON dimension declaration)"
      )
    Nothing ->
      let dims =
              traverse (resolveDimensionExpFunctionCall symTable) (aStrip args)
          -- Set default kind; there is no way to know it at this point
          entry = SVariable (TArray (TInteger 4) dims) (symbol, 0)
      in  M.insert symbol entry symTable
    _ -> error
      (symbol
      ++ " was found in SymbolTable. This case is not possible, \
                 \because ExpFunctionCall as dimension declarator can only occur for \
                 \variables that occur after COMMON block in the code"
      )

-- | Given symbol, list of Indices and SymbolTable, it updates the relevant
-- variable in the SymbolTable to become an array with dimensions described by
-- the list of Indices.
--
-- This function is needed to handle dimensions specifications within COMMONs,
-- because fortran-src doesn't support 'DimensionDeclarator's within COMMON blocks.
updateDimensionExpSubscript
  :: Name -> AList Index (Analysis a) -> SymbolTable -> SymbolTable
updateDimensionExpSubscript symbol indices symTable =
  case M.lookup symbol symTable of
    Just (SVariable TArray{} _) -> error
      (symbol
      ++ " is array-typed VaribleEntry. \
                 \Invalid fortran syntax (Duplicate DIMENSION attribute)"
      )
    Just (SVariable std loc) ->
      let dims =
              traverse (resolveDimensionExpSubscript symTable) (aStrip indices)
          entry = SVariable (TArray std dims) loc
      in  M.insert symbol entry symTable
    Just (SDummy _) -> error
      (symbol
      ++ " is DummyVariableEntry. \
                 \Invalid fortran syntax (Dummy in COMMON dimension declaration)"
      )
    Nothing ->
      let dims =
              traverse (resolveDimensionExpSubscript symTable) (aStrip indices)
          -- Set default kind; there is no way to know it at this point
          entry = SVariable (TArray (TInteger 4) dims) (symbol, 0)
      in  M.insert symbol entry symTable
    _ -> error "Invalid fortran syntax"

handleCommon
  :: Data a => SymbolTable -> AList CommonGroup (Analysis a) -> SymbolTable
handleCommon symTable alist = foldl' f symTable (aStrip alist)
 where
  f symt (CommonGroup _ _ _ alist2) = foldl' f2 symt (aStrip alist2)
   where
    f2 symt2 (ExpFunctionCall _ _ varExp (Just alist3)) =
      updateDimensionExpFunctionCall (srcName varExp) alist3 symt2
    f2 symt2 (ExpSubscript _ _ varExp alist3) =
      updateDimensionExpSubscript (srcName varExp) alist3 symt2
    f2 symt2 _ = symt2

-- | Given a 'SymbolTable' and a 'Statement' found in a 'ProgramUnit', return a new 'SymbolTable'
-- with any newly defined symbols
stSymbols :: Data a => SymbolTable -> Statement (Analysis a) -> SymbolTable
stSymbols symTable (StParameter _ _ alist) = handleParameter symTable alist
stSymbols symTable (StDeclaration _ _ typespec _ decls) =
  handleDeclaration symTable typespec decls
stSymbols symTable (StDimension _ _ decls     ) = handleDimension symTable decls
stSymbols symTable (StCommon    _ _ alist     ) = handleCommon symTable alist
stSymbols symTable (StInclude _ _ _ (Just bls)) = foldl' blSymbols symTable bls
stSymbols symTable _                            = symTable

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
  getName (DeclArray _ _ (ExpValue _ _ (ValVariable str)) _ _ _) = str
  getName (DeclVariable _ _ (ExpValue _ _ (ValVariable str)) _ _) = str
  getName _ = error "Unexpected declaration expression"
  toType (DeclArray _ _ _ dims _ _) =
    typeSpecToArrayType symt (aStrip dims) tyspec
  toType DeclVariable{} = typeSpecToScalarType symt tyspec
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
