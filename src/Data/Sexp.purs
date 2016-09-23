-- | S-expressions.
module Data.Sexp
( Sexp
) where

import Data.List (List)
import Prelude

-- | S-expression.
data Sexp = Atom String | List (List Sexp)

derive instance eqSexp :: Eq Sexp
derive instance ordSexp :: Ord Sexp
