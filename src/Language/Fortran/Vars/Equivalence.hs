module Language.Fortran.Vars.Equivalence
  ( processEquivalence
  )
where

import           Data.Data                      ( Data )
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromMaybe )
import           Language.Fortran.Analysis      ( Analysis
                                                , srcName
                                                )
import           Language.Fortran.AST           ( AList
                                                , aStrip
                                                , Expression
                                                , Statement(..)
                                                )

import           Language.Fortran.Vars.MemoryLocation
                                                ( getLocation )
import           Language.Fortran.Vars.Types    ( Location
                                                , ProgramUnitModel
                                                )
import           Language.Fortran.Vars.Union    ( union )

associate :: ProgramUnitModel -> [Location] -> ProgramUnitModel
associate puModel locations =
  let firstLoc : restLocs = locations
      f (model, loc) = union model loc
      (puModel', _) = foldl' f (puModel, firstLoc) restLocs
  in  puModel'

equivalence
  :: Data a => ProgramUnitModel -> Statement (Analysis a) -> ProgramUnitModel
equivalence puModel0 (StEquivalence _ ss equivsList) = foldl'
  f
  puModel0
  (aStrip equivsList)
 where
  f
    :: Data a
    => ProgramUnitModel
    -> AList Expression (Analysis a)
    -> ProgramUnitModel
  f model@(symTable, _) equivs =
    let
      locations =
        (\x ->
            fromMaybe
                (error $ "Couldn't calculate location at " <> show ss <> srcName x
                )
              $ getLocation symTable x
          )
          <$> aStrip equivs
    in  associate model locations
equivalence model _ = model

-- | Given a list of all the 'Statement's in the program and a 'ProgramUnitModel', produce a new
-- 'ProgramUnitModel' that accounts for any of the FORTRAN equivalences that were found in the
-- 'Language.Fortran.AST.ProgramUnit'
processEquivalence
  :: Data a => [Statement (Analysis a)] -> ProgramUnitModel -> ProgramUnitModel
processEquivalence stmts puModel = foldl' equivalence puModel eqvStmts
  where eqvStmts = [ s | s@StEquivalence{} <- stmts ]
