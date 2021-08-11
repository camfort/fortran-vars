module Language.Fortran.Vars.Range
  ( Range
  , overlap
  , anyOverlap
  )
where

-- | Inclusive range used to represent a piece of memory
type Range = (Int, Int)   -- ^ (start, end)

-- | Return True if two ranges have overlap
overlap :: Range -> Range -> Bool
overlap (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

-- | Return True if a given 'Range' overlaps any 'Range' in the
-- provided list
anyOverlap :: Range -> [Range] -> Bool
anyOverlap range = any (overlap range)
