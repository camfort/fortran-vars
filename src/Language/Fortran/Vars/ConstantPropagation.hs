{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.Vars.ConstantPropagation
  ( constantPropagationValue
  , ValueOf
  )
where

import           Language.Fortran.Vars ( programFileModel )
import           Language.Fortran.Vars.Types
import           Language.Fortran.Vars.Kind
                                                ( getTypeKind )
import           Language.Fortran.Vars.Range
                                                ( Range
                                                , overlap
                                                , anyOverlap
                                                )
import           Language.Fortran.Vars.Eval.Deprecated.Operation
                                                ( valueToExpVal )
import           Language.Fortran.Vars.MemoryLocation
                                                ( generateLinearizedIndexRange )
import           Language.Fortran.Vars.CPValue
                                                ( CPValue(..)
                                                , meet
                                                , unaryOper
                                                , binaryOper
                                                , isTop
                                                , isBot
                                                , isConstInt
                                                )

import           Language.Fortran.AST
import           Language.Fortran.Analysis      ( BBGr(..)
                                                , Analysis(..)
                                                , srcName
                                                )
import           Language.Fortran.Util.Position ( SrcSpan
                                                , getSpan
                                                )
import           Language.Fortran.Analysis.BBlocks
                                                ( BBlockMap
                                                , genBBlockMap
                                                , analyseBBlocks
                                                )
import           Language.Fortran.Analysis.DataFlow
                                                ( InOutMap
                                                , dataFlowSolver
                                                , revPostOrder
                                                )
import qualified Data.Map                      as M
import qualified Data.IntMap                   as IM
import           Data.List                      ( foldl' )
import           Data.Graph.Inductive.Graph     ( Node
                                                , lab
                                                , pre
                                                , labNodes
                                                )
import           Data.Data                      ( Data )
import           Data.Generics.Uniplate.Data
import           Data.Maybe                     ( maybeToList
                                                , fromMaybe
                                                , fromJust
                                                )

-- | ValueOf is a closure that takes an 'Expression' and deduces its 'CPValue'
type ValueOf a = Expression (Analysis a) -> CPValue

-- | MemoryTable represents the determined memory state of a particular memory
-- block. It contains the mappings between a piece of memory and its 'CPValue'.
-- The piece of memory is represented by an inclusive range inside a 'MemoryBlock'
type MemoryTable = M.Map Range CPValue

-- | MemoryTables contains 'MemoryTable's for all 'MemoryBlock's
type MemoryTables = M.Map MemoryBlockName MemoryTable

-- | Collection of output of constant propagation analysis ('InOutMap' 'MemoryTables')
-- for each ProgramUnit
type MemoryTablesMap = M.Map ProgramUnitName (InOutMap MemoryTables)

-- | Given a 'SymbolTable', 'MemoryTables', a possible specification of the
-- beginning of the substring as well as one for the end, and the length of
-- the original string, generate the bounds that should be used for the
-- substring. If the bounds cannot be determined the extraction will return
-- 'Nothing'.
extractSubstringBounds
  :: SymbolTable
  -> MemoryTables
  -> Maybe (Expression (Analysis a))
  -> Maybe (Expression (Analysis a))
  -> Int
  -> Maybe (Int, Int)
extractSubstringBounds symTable memTables mb me l =
  let f d m = case m >>= \v -> Just $ cpValue symTable memTables v of
        Nothing              -> Just d
        Just (Const (Int v)) -> Just v
        Just _               -> Nothing
  in  case (f 1 mb, f l me) of
        (Just b, Just e) -> Just (b, e)
        _                -> Nothing

-- | Given a 'SymbolTable', 'MemoryTables', the base 'Expression' of the substring,
-- any 'Index's that accompany the base, and possible the specifications for
-- the beginning and end of the substring, return the 'CPValue' of the substring.
substringCPValue
  :: SymbolTable
  -> MemoryTables
  -> Expression (Analysis a)
  -> [Index (Analysis a)]
  -> Maybe (Expression (Analysis a))
  -> Maybe (Expression (Analysis a))
  -> CPValue
substringCPValue symTable memTables e is mb me =
  let name           = srcName e
      isArraySection = case fromJust $ M.lookup name symTable of
        (SVariable (TArray _ dims) _) -> length is < dimsLength dims
        _                             -> False
  in  if isArraySection
        then errArraySection
        else case lookupArray symTable memTables name is of
          Top -> Top
          Bot -> Bot
          Const (Str s) ->
            case extractSubstringBounds symTable memTables mb me $ length s of
              Just (b', e') ->
                Const . Str $ take (e' - b' + 1) $ drop (b' - 1) s
              Nothing -> Bot
          _ -> errArraySection
  where
    errArraySection = error "Array sections are not allowed in FORTRAN 77"

-- | Given 'SymbolTable', 'MemoryTables' and an 'Expression', determine the 'CPValue'
-- of the 'Expression'
cpValue :: SymbolTable -> MemoryTables -> Expression (Analysis a) -> CPValue
cpValue symTable memTables expr = case expr of
  ExpValue _ _ ValVariable{} -> lookupName symTable memTables $ srcName expr
  ExpValue _ s val -> Const (valueToExpVal s val)
  ExpUnary _ _ op e -> let v = cpValue symTable memTables e in unaryOper op v
  ExpBinary _ _ op e1 e2 ->
    let v1 = cpValue symTable memTables e1
        v2 = cpValue symTable memTables e2
    in  binaryOper op v1 v2
  ExpSubscript _ _ e@ExpValue{} (AList _ _ [IxRange _ _ mb me _]) ->
    substringCPValue symTable memTables e [] mb me
  ExpSubscript _ _ e@ExpValue{} is ->
    lookupArray symTable memTables (srcName e) $ aStrip is
  ExpSubscript _ _ (ExpSubscript _ _ e@ExpValue{} is) (AList _ _ [IxRange _ _ mb me _])
    -> substringCPValue symTable memTables e (aStrip is) mb me
  ExpFunctionCall{} -> Bot
  _ -> error $ "Evaluation of the expression is not implemented - " ++ show
    (getSpan expr)

-- | A piece of memory is represented as inclusive range indicating the start and
-- end offset of the memory within a memory block. This function is used for scalar
-- variable.
getVariableMemory :: SymbolTable -> Name -> Maybe (MemoryBlockName, Range)
getVariableMemory symTable name = case M.lookup name symTable of
  -- Array pointer passed to subroutine/function (thus treated as 'ValVariable')
  Just (SVariable (TArray ty dims) (memBlockName, offset)) -> do
    kind  <- getTypeKind ty
    case dimsTraverse dims of
      Just (DimsExplicitShape ds) ->
        Just (memBlockName, (offset, offset + sizeOfArray kind ds - 1))
      _ -> Nothing
  Just (SVariable ty (memBlockName, offset)) -> do
    kind <- getTypeKind ty
    Just (memBlockName, (offset, offset + kind - 1))
  Just SParameter{} -> Nothing
  Just SDummy{}     -> Nothing
  Nothing           -> Nothing
  _                 -> error $ "getVariableMemory -  not a variable : " ++ name

-- | Given a 'CPValue' without any present 'Bot' or 'Top'
-- 'CPValue's, convert to an 'Int'
unsafeStripIndexCP :: CPValue -> Int
unsafeStripIndexCP (Const (Int i)) = i
unsafeStripIndexCP _ = error "Expected no Top, Bot, nor non Int values"

-- | This data type describes the memory layout of an array's memory with
-- some additional qualification.
--
-- If any of the indices is unknown then the range of the array expression is
-- unknown. In this case 'UnknownIndices' variant is used with the range set
-- as the whole range of the array.
--
-- On the other hand, if indices are known constant, then the exact
-- range for the indices is used with the 'ConstantIndices' variant.
--
-- If the variable is a string declared as a scalar, the 'UnknownIndices' variant is
-- used with the range set to the size of the variable.
data ArrayMemory
  = UnknownIndices (MemoryBlockName, Range)
  | ConstantIndices (MemoryBlockName, Range)

-- | This function is used for array variable. It returns the range of the
-- specified element.
getArrayMemory
  :: SymbolTable
  -> MemoryTables
  -> Name
  -> [Index (Analysis a)]
  -> Maybe ArrayMemory
getArrayMemory symTable memTables name indices =
    case M.lookup name symTable of
      Nothing -> error $ "variable not in symbol table: "<>name
      Just entry ->
        case entry of
          SVariable ty (memBlockName, start) ->
            case ty of
              TArray _ dims' ->
                case dimsTraverse dims' of -- only handle static
                  Just (DimsExplicitShape dims)
                    | any isBot idxCPValues
                    -> UnknownIndices . (memBlockName, ) <$> arrayRange
                    | any isTop idxCPValues
                    -> UnknownIndices . (memBlockName, ) <$> arrayRange
                    | not (all isConstInt idxCPValues)
                    -> UnknownIndices . (memBlockName, ) <$> arrayRange
                    | otherwise
                    -> do
                      let is = map unsafeStripIndexCP idxCPValues
                      range <- generateLinearizedIndexRange is start <$> pure dims <*> kind
                      Just $ ConstantIndices (memBlockName, range)
                   where
                    kind       = getTypeKind ty
                    size       = sizeOfArray <$> kind <*> pure dims
                    arrayRange = (\x -> (start, start + x - 1)) <$> size

                  -- only handle explicit-shape arrays
                  Just{} -> Nothing
                  Nothing -> Nothing
              _
                | null indices -> ConstantIndices . (memBlockName, ) <$> range
                | otherwise    -> UnknownIndices . (memBlockName, ) <$> range
               where
                kind  = getTypeKind ty
                range = (\x -> (start, start + x - 1)) <$> kind
          _ -> Nothing
  where
    idxCPValues = cpValueOfIndices symTable memTables indices

-- | Internal function to find 'CPValue' of a symbol
lookupName :: SymbolTable -> MemoryTables -> Name -> CPValue
lookupName symTable memTables name = case M.lookup name symTable of
  Just (SParameter _  val) -> Const val
  Just (SVariable  ty loc) -> lookupScalarVariable memTables ty loc
  Just SDummy{}            -> Bot
  Just SExternal{}         -> Bot
  Nothing                  -> Bot

-- | Internal function to find 'CPValue' of a scalar variable
lookupScalarVariable :: MemoryTables -> Type -> Location -> CPValue
lookupScalarVariable memTables ty loc =
  let (memBlockName, start) = loc
      mkind                 = getTypeKind ty
      mrange                = (\x -> (start, start + x - 1)) <$> mkind
  in  case mrange of
        Just range -> case M.lookup memBlockName memTables of
          Just memTbl -> lookupRange range memTbl
          Nothing     -> Top
        Nothing -> Bot

-- | Internal fucntion to find 'CPValue' of an array element
lookupArray
  :: SymbolTable -> MemoryTables -> Name -> [Index (Analysis a)] -> CPValue
lookupArray symTable memTables name indices =
  case getArrayMemory symTable memTables name indices of
    Nothing                 -> Bot
    Just (UnknownIndices _) -> Bot
    Just (ConstantIndices (memBlockName, range)) ->
      case M.lookup memBlockName memTables of
        Just memTbl -> lookupRange range memTbl
        Nothing     -> Top

-- | Internal function to resovle the 'CPValue's of array indices
cpValueOfIndices
  :: SymbolTable -> MemoryTables -> [Index (Analysis a)] -> [CPValue]
cpValueOfIndices symTable memTables = map cpValueOfIndex
 where
  cpValueOfIndex :: Index (Analysis a) -> CPValue
  cpValueOfIndex (IxSingle _ _ _ e) = cpValue symTable memTables e
  cpValueOfIndex _ = error "Array sections are not allowed in FORTRAN 77"


-- | Internal function to look up the 'CPValue' of a 'Range'
lookupRange :: Range -> MemoryTable -> CPValue
lookupRange range memTable = case M.lookup range memTable of
  Just val -> val
  Nothing | anyOverlap range (M.keys memTable) -> Bot
  _        -> Top


-- | Constant Propagation Analysis
-- Given a 'ProgramUnitModel' and a control flow graph (basic blocks graph) of a
-- 'ProgramUnit', returns the In and Out 'MemoryTables' for each node in the graph
constantPropagationAnalysis
  :: ProgramUnitModel -> BBGr (Analysis a) -> InOutMap MemoryTables
constantPropagationAnalysis puModel gr = dataFlowSolver
  gr
  (const (M.empty, M.empty))
  revPostOrder
  inn
  out
 where
  inn outF b =
    M.unionsWith (M.unionWith meet) [ outF s | s <- pre (bbgrGr gr) b ]
  out innF b = foldl' (varDefine puModel)
                      (innF b)
                      (fromJustMsg "constantPropagation" $ lab (bbgrGr gr) b)

-- | Given 'MemoryTables', a 'MemoryBlockName', and a 'Range',
-- update that 'Range' to contain the specified 'CPValue'. If
-- the boolean argument is set to true, delete any values in
-- overlapping ranges
updateRangeValue
  :: MemoryTables -> MemoryBlockName -> Range -> CPValue -> Bool -> MemoryTables
updateRangeValue memTables' memBlockName range val filt =
  let rangeMap  = fromMaybe M.empty $ M.lookup memBlockName memTables'
      rangeMap' = if filt
        then M.filterWithKey (\k _ -> not (overlap k range)) rangeMap
        else rangeMap
      newRangeMap = M.insert range val rangeMap'
  in  M.insert memBlockName newRangeMap memTables'

-- | Process the definition of a substring in a block and update
-- the substring's 'CPValue' in the corresponding 'MemoryTable'.
--
-- Aside from the 'SymbolTable' and 'MemoryTable', this
-- function requires the base 'Expression' of the substring
-- as well as any 'Index's that go along with that base,
-- possibly a specification of the beginning and end of the
-- array, and finally the 'Expression' that is being assigned.
substringDefine
  :: SymbolTable
  -> MemoryTables
  -> Expression (Analysis a)
  -> [Index (Analysis a)]
  -> Maybe (Expression (Analysis a))
  -> Maybe (Expression (Analysis a))
  -> Expression (Analysis a)
  -> MemoryTables
substringDefine symTable memTables e is mb me rhs =
  case getArrayMemory symTable memTables (srcName e) is of
    Nothing -> memTables
    Just (UnknownIndices (memBlockName, wholeArrayRange)) ->
      let val = Bot
      in  updateRangeValue memTables memBlockName wholeArrayRange val True
    Just (ConstantIndices (memBlockName, range)) ->
      let val = cpValue symTable memTables rhs
          str = case M.lookup memBlockName memTables of
            Just memTbl -> lookupRange range memTbl
            Nothing     -> Top
          val' = case val of
            Const (Str s) -> case str of
              Const (Str o) ->
                case
                    extractSubstringBounds symTable memTables mb me $ length o
                  of
                    Just (b', e') ->
                      let s' = s ++ replicate (e' - b' + 1 - length s) ' '
                      in  Const . Str $ take (b' - 1) o ++ s' ++ drop e' o
                    Nothing -> Bot
              _ -> Bot
            _ -> Bot
      in  updateRangeValue memTables memBlockName range val' True

-- | Process the definition of a variable in a block and update the
-- variable's 'CPValue' in the corresponding 'MemoryTable'.
varDefine
  :: ProgramUnitModel -> MemoryTables -> Block (Analysis a) -> MemoryTables
varDefine (symTable, storageTable) memTables (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | expr@(ExpValue _ _ ValVariable{}) <- lhs
  , not $ inCommon expr
  = let name = srcName expr
    in
      case getVariableMemory symTable name of
        Just (memBlockName, range@(b, e)) ->
          let val = cpValue symTable memTables rhs
          in
            case fromJust $ M.lookup name symTable of
              SVariable (TArray ty _) _ ->
                let
                  kind =
                    fromMaybe (error $ "Couldn't get kind of type " <> show ty)
                      $ getTypeKind ty
                  val' = case val of
                    Const (Str s) -> Const . Str . replicate kind $ if null s
                      then ' '
                      else head s
                    _ -> val
                  handler mt range' =
                    updateRangeValue mt memBlockName range' val' True
                in
                  foldl' handler
                         memTables
                         [ (b', b' + kind - 1) | b' <- [b, b + kind .. e] ]
              _ -> updateRangeValue memTables memBlockName range val True
        Nothing -> memTables
  | ExpSubscript _ _ e@ExpValue{} (AList _ _ [IxRange _ _ mb me _]) <- lhs
  , not $ inCommon e
  = substringDefine symTable memTables e [] mb me rhs
  | ExpSubscript _ _ e@ExpValue{} indices <- lhs
  , not $ inCommon e
  = case getArrayMemory symTable memTables (srcName e) (aStrip indices) of
    Nothing -> memTables
    Just (UnknownIndices (memBlockName, wholeArrayRange)) ->
      let val = Bot
      in  updateRangeValue memTables memBlockName wholeArrayRange val True
    Just (ConstantIndices (memBlockName, range)) ->
      let val = cpValue symTable memTables rhs
      in  updateRangeValue memTables memBlockName range val False
  | ExpSubscript _ _ (ExpSubscript _ _ e@ExpValue{} is) (AList _ _ [IxRange _ _ mb me _]) <-
    lhs
  , not $ inCommon e
  = substringDefine symTable memTables e (aStrip is) mb me rhs
  | otherwise
  = memTables
 where
  inCommon :: Expression (Analysis a) -> Bool
  inCommon e = case M.lookup (srcName e) symTable of
    Just (SVariable _ (memBlockName, _)) ->
      case M.lookup memBlockName storageTable of
        Just memBlock -> storageClass memBlock == Common
        _             -> False
    _ -> False

varDefine _ memTables _ = memTables


-- | ExpressionContext represents the block, basic block and 'ProgramUnit'
-- in which an expression is located
type ExpressionContext = (Node, Node, ProgramUnitName)

-- | Mapping from expression source span to 'ExpressionContext'
type ExpressionContextMap = M.Map SrcSpan ExpressionContext


-- | Look up the 'ExpressionContext' of an 'Expression'
lookupExpressionContext
  :: Data a => Expression a -> ExpressionContextMap -> ExpressionContext
lookupExpressionContext expr exprCxtMap =
  let s   = getSpan expr
      err = "Lookup Expression Context at " ++ show s
  in  fromJustMsg err $ M.lookup s exprCxtMap

-- | Given 'BBlockMap', generates 'ExpressionContextMap'
genExpressionContextMap
  :: Data a => BBlockMap (Analysis a) -> ExpressionContextMap
genExpressionContextMap bblockMap =
  M.fromList
    . reverse
    $ [ (getSpan expr, (bl, bbl, pu))
      | (pu , gr        ) <- M.toList bblockMap
      , (bbl, basicBlock) <- labNodes (bbgrGr gr)
      , block             <- basicBlock
      , bl                <- maybeToList . label $ block
      , expr              <- allExp block
      ]
 where
  allExp :: Data a => Block (Analysis a) -> [Expression (Analysis a)]
  allExp = universeBi


-- | Determine the 'CPValue' of an expression in a 'ProgramUnit' using constant
-- propagation analysis
--
-- Input:
--
--   * 'ProgramUnitModel'
--   * 'BBGr' -  Control Flow Graph of Basic Blocks
--   * 'InOutMap' 'MemoryTables' - generated by constantPropagationAnalysis
--   * 'ExpressionContextMap' - to identify the block and basic block of the input expression
--   * 'Expression'
--
-- Output:
--
--   * 'CPValue'
--
-- Description:
--
--   The control flow graph and the associated In and Out 'MemoryTables' are at the
--   level of Basic Block, which is coarse-grained with regard to Expression.
--   The memory state at the beginning of basic block may not represent
--   the memory state at the site of the expression. To get more precise determination
--   of value, the 'MemoryTables' is updated by processing each blocks preceding
--   the enclosing block of input expression.
--
--   The updated 'MemoryTables' is then used to determine the value of the 'Expression'.
constantPropagationValuePU
  :: Data a
  => ProgramUnitModel
  -> BBGr (Analysis a)
  -> InOutMap MemoryTables
  -> ExpressionContext
  -> Expression (Analysis a)
  -> CPValue
constantPropagationValuePU puModel@(symTable, _) gr memTables exprCxt expr =
  let
    (expBlock, expBBlock, _) = exprCxt
    bblock = fromJustMsg "Basic Block" $ lab (bbgrGr gr) expBBlock
    precedingBlocks = takeWhile (\b -> fromJust (label b) /= expBlock) bblock
    inBBMemoryTables = fst . fromJustMsg "Cannot find MemoryTables" $ IM.lookup
      expBBlock
      memTables
    inBlockMemoryTables =
      foldl' (varDefine puModel) inBBMemoryTables precedingBlocks
  in
    cpValue symTable inBlockMemoryTables expr

-- | Internal function to determine the 'CPValue' of an expression in a 'ProgramFile'
-- using constantpropagation analysis
constantPropagationValuePF
  :: Data a
  => ProgramFileModel
  -> BBlockMap (Analysis a)
  -> MemoryTablesMap
  -> ExpressionContextMap
  -> Expression (Analysis a)
  -> CPValue
constantPropagationValuePF pfModel bbgraphs memTablesMap exprCtxMap expr =
  let exprCtx@(_, _, unitName) = lookupExpressionContext expr exprCtxMap
      puModel = fromJustMsg "Find SymbolTable" $ M.lookup unitName pfModel
      controlFlowGraph =
          fromJustMsg "Find basic block graph" $ M.lookup unitName bbgraphs
      memTables = fromJustMsg "Find MemTables" $ M.lookup unitName memTablesMap
  in  constantPropagationValuePU puModel controlFlowGraph memTables exprCtx expr


-- | Given a 'ProgramFile', return 'ValueOf' closure, which determines
-- whether given 'Expression' can be evaluated statically to a constant value using
-- constant propagation analysis.
--
-- Usage:
--   The best approach is to create a closure first as illustrated in the following
--   code example, so only one run of constant propragation analysis is performed
--   for a 'ProgramFile'.
--
--   @
--   let cpValueOf = constantPropagationValue pf
--   ...
--   in
--       ...
--       cpVauleOf e1
--       cpValueOf e2
--   @
constantPropagationValue :: Data a => ProgramFile (Analysis a) -> ValueOf a
constantPropagationValue pf =
  let pfb      = analyseBBlocks pf
      pfModel  = programFileModel pfb
      bbgraphs = genBBlockMap pfb
      mapFunc puName controlFlowGraph =
          let puModel =
                  fromJustMsg "Find ProgramUnitModel" $ M.lookup puName pfModel
          in  constantPropagationAnalysis puModel controlFlowGraph
      memTablesMap = M.mapWithKey mapFunc bbgraphs
      exprCtxMap   = genExpressionContextMap bbgraphs
  in  constantPropagationValuePF pfModel bbgraphs memTablesMap exprCtxMap

-- Utility functions
fromJustMsg :: String -> Maybe a -> a
fromJustMsg _   (Just x) = x
fromJustMsg msg _        = error msg

-- | 'Language.Fortran.Analysis.BBlocks.analyseBBlocks' annotates the 'Block'
-- and 'Expression' AST node with unique integer label. This function retrieves
-- the label from an AST node if the label exsits.
label
  :: forall a b
   . (Data a, Data (b (Analysis a)), Annotated b)
  => b (Analysis a)
  -> Maybe Int
label = insLabel . getAnnotation

-- | Given kind and dimensions, calculate the size of an array
sizeOfArray :: Foldable t => Int -> t (Dim Int) -> Int
sizeOfArray kind dims = kind * arraySize
  where
    arraySize = foldl' (\acc (Dim l h) -> acc * (h - l + 1)) 1 dims
