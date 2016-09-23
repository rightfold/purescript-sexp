-- | S-expressions.
module Data.Sexp
( Sexp
, class ToSexp, toSexp
, gToSexp
) where

import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.List ((:), List(..))
import Data.List as List
import Data.String as String
import Prelude

-- | S-expression.
data Sexp = Atom String | List (List Sexp)

derive instance eqSexp :: Eq Sexp
derive instance ordSexp :: Ord Sexp
derive instance genericSexp :: Generic Sexp

class ToSexp a where
  toSexp :: a -> Sexp

instance toSexpVoid :: ToSexp Void where
  toSexp = absurd

instance toSexpUnit :: ToSexp Unit where
  toSexp _ = List Nil

instance toSexpBoolean :: ToSexp Boolean where
  toSexp true = Atom "true"
  toSexp false = Atom "false"

instance toSexpInt :: ToSexp Int where
  toSexp n = Atom (show n)

instance toSexpNumber :: ToSexp Number where
  toSexp n = Atom (show n)

instance toSexpString :: ToSexp String where
  toSexp = Atom

instance toSexpArray :: (ToSexp a) => ToSexp (Array a) where
  toSexp xs = List (List.fromFoldable (map toSexp xs))

instance toSexpGenericSpine :: ToSexp GenericSpine where
  toSexp SUnit        = List (Atom "SUnit"    : Nil)
  toSexp (SArray xs)  = List (Atom "SArray"   : List.fromFoldable (map (\thk -> toSexp (thk unit)) xs))
  toSexp (SChar c)    = List (Atom "SChar"    : Atom (String.singleton c) : Nil)
  toSexp (SString s)  = List (Atom "SString"  : Atom s : Nil)
  toSexp (SInt i)     = List (Atom "SInt"     : Atom (show i) : Nil)
  toSexp (SBoolean b) = List (Atom "SBoolean" : Atom (show b) : Nil)
  toSexp (SNumber n)  = List (Atom "SNumber"  : Atom (show n) : Nil)
  toSexp (SRecord r)  = List (Atom "SRecord"  : List.fromFoldable (r >>= \{recLabel, recValue} -> [Atom recLabel, toSexp (recValue unit)]))
  toSexp (SProd c xs) = List (Atom "SProd"    : Atom c : List.fromFoldable (map (\thk -> toSexp (thk unit)) xs))

-- | Convert anything to an S-expression.
gToSexp :: forall a. (Generic a) => a -> Sexp
gToSexp = toSpine >>> toSexp
