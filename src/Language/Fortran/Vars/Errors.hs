module Language.Fortran.Vars.Errors
  ( invalidArg'
  , invalidArg
  )
where

import           Data.List                      ( foldl' )

-- | Given a function name and arguments, generate an
-- error message for invalid arguments
genError :: Show a => String -> [a] -> String
genError funcName args =
  let
    argMsg = case args of
      []     -> ""
      [ x ]  -> " - " ++ show x
      x : xs -> "s - " ++ foldl' (\acc y -> acc ++ ", " ++ show y) (show x) xs
  in  funcName ++ " : invalid argument" ++ argMsg

-- | Function to return an error case when an unappropriate arguemnt is
-- passed to some function or when some pattern match issue occurs
invalidArg' :: Show a => String -> [a] -> Either String b
invalidArg' funcName args = Left $ genError funcName args

-- | Function to throw an error when an unappropriate arguemnt is
-- passed to some function or when some pattern match issue occurs
invalidArg :: Show a => String -> [a] -> b
invalidArg funcName args = error $ genError funcName args
