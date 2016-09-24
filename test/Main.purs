module Test.Main
( main
) where

import Benchmark (benchmark')
import Benchmark.Plot.Gnuplot (gnuplot)
import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core as AC
import Data.Argonaut.Parser as AP
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.List ((:), List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Sexp
import Data.Tuple (Tuple)
import Prelude
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Laws.Data.Sexp (checkAsSexp)
import Type.Proxy (Proxy(..))

data GenericTest
  = A
  | B String
  | C {x :: Int, y :: Number}
  | D GenericTest

derive instance eqGenericTest :: Eq GenericTest
derive instance genericGenericTest :: Generic GenericTest
instance toSexpGenericTest :: ToSexp GenericTest where toSexp = gToSexp
instance fromSexpGenericTest :: FromSexp GenericTest where fromSexp = gFromSexp
instance asSexpGenericTest :: AsSexp GenericTest

instance arbitraryGenericTest :: Arbitrary GenericTest where
  arbitrary = Gen.sized arbitrary'
    where
    arbitrary' 0 = pure A
    arbitrary' 1 = B <$> arbitrary
    arbitrary' n
      | n `mod` 2 == 0 = (\x y -> C {x, y}) <$> arbitrary <*> arbitrary
      | otherwise      = D <$> arbitrary' (n / 2)

main = do
  checkAsSexp (Proxy :: Proxy Sexp)
  checkAsSexp (Proxy :: Proxy Unit)
  checkAsSexp (Proxy :: Proxy Boolean)
  checkAsSexp (Proxy :: Proxy Char)
  checkAsSexp (Proxy :: Proxy Int)
  checkAsSexp (Proxy :: Proxy Number)
  checkAsSexp (Proxy :: Proxy String)
  checkAsSexp (Proxy :: Proxy (Array Sexp))
  checkAsSexp (Proxy :: Proxy Ordering)
  checkAsSexp (Proxy :: Proxy (List Sexp))
  checkAsSexp (Proxy :: Proxy (Maybe Sexp))
  checkAsSexp (Proxy :: Proxy (Tuple Int Sexp))
  checkAsSexp (Proxy :: Proxy (Either Int Sexp))
  checkAsSexp (Proxy :: Proxy GenericTest)

  log "Checking toString and fromString"
  quickCheck' 1000 \sexp -> Just sexp == fromString (toString sexp)

  log "Benchmarking against JSON"
  benchJSON <- benchmark' 10 1000 \i -> pure $ AP.jsonParser (AC.stringify (genJSON i))
  benchSexp <- benchmark' 10 1000 \i -> pure $ fromString (toString (genSexp i))
  log $ gnuplot [ {title: "JSON", benchmark: benchJSON}
                , {title: "Sexp", benchmark: benchSexp}
                ]

genJSON :: Int -> AC.Json
genJSON 0 = AC.fromString "hello world"
genJSON n = AC.fromArray $ List.toUnfoldable $ go (n - 1)
  where
  go 0 = Nil
  go n = genJSON (n / 2) : go (n / 2)

genSexp :: Int -> Sexp
genSexp 0 = Atom "hello world"
genSexp n = List $ go (n - 1)
  where
  go 0 = Nil
  go n = genSexp (n / 2) : go (n / 2)
