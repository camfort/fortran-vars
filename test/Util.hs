module Util where

import Language.Fortran.Common.Array

-- | Explicit-shape array, one dimension with both bounds statically specified.
dess1 :: Applicative t => a -> a -> Dims t (Maybe a)
dess1 lb ub = DimsExplicitShape $ pure $ Dim (Just lb) (Just ub)

-- | Explicit-shape array, one dimension, lower and upper bounds provided.
--
-- Slight overkill type. Simplifiy if you start getting ambiguity.
des1 :: Applicative t => a -> a -> Dims t a
des1 lb ub = DimsExplicitShape $ pure $ Dim lb ub

-- | Explicit-shape array, one dimension, lower bound 1, upper bound provided.
--
-- Slight overkill type. Simplifiy if you start getting ambiguity.
des1' :: (Applicative t, Applicative f, Num a) => f a -> Dims t (f a)
des1' ub = DimsExplicitShape $ pure $ Dim (pure 1) ub
