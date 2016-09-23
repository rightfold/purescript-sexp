module Test.QuickCheck.Laws.Data.Sexp
( checkAsSexp
) where

import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Data.Sexp (class AsSexp, fromSexp, toSexp)
import Prelude
import Test.QuickCheck (QC, quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Type.Proxy (Proxy)

checkAsSexp
  :: forall eff a
   . (Arbitrary a, AsSexp a, Eq a)
  => Proxy a
  -> QC eff Unit
checkAsSexp _ = do
  log "Checking 'Losslessness' law for AsSexp"
  quickCheck \(x :: a) -> fromSexp (toSexp x) == Just x
