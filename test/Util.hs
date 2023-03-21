module Util where

import Language.Fortran.Common.Array

des1 :: Applicative t => a -> a -> Dims t a
des1 lb ub = DimsExplicitShape $ pure $ Dim lb ub
