module Test.Main
( main
) where

import Data.Maybe (Maybe(..))
import Data.Sexp (fromString, toString)
import Prelude
import Test.QuickCheck (quickCheck')

main = do
  quickCheck' 1000 \sexp -> Just sexp == fromString (toString sexp)
