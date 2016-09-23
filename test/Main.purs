module Test.Main
( main
) where

import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Data.List ((..))
import Data.Maybe (Maybe(..))
import Data.Sexp (gToSexp, toSexp, toString)
import Data.Tuple (Tuple(..))
import Prelude

main = do
  example $ toSexp unit
  example $ toSexp true
  example $ toSexp 1
  example $ toSexp 1.0
  example $ toSexp "foo"
  example $ toSexp [1, 2, 3]
  example $ toSexp EQ
  example $ toSexp (1 .. 10)
  example $ toSexp (Just 1)
  example $ toSexp (Just (Left (Tuple 1 (Nothing :: Maybe String)) :: Either _ Int))
  example $ gToSexp (Just (Left (Tuple 1 (Nothing :: Maybe String)) :: Either _ Int))

  where
  example = log <<< toString
