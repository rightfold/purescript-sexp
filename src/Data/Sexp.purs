-- | S-expressions.
module Data.Sexp
( Sexp
, toString
, class ToSexp, toSexp
, gToSexp
) where

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Prelude

-- | S-expression.
data Sexp = Atom String | List (List Sexp)

derive instance eqSexp :: Eq Sexp
derive instance ordSexp :: Ord Sexp
derive instance genericSexp :: Generic Sexp

toString :: Sexp -> String
toString (Atom s)  = show s
toString (List Nil) = "()"
toString (List (x : xs)) = "(" <> toString x <> foldMap (\e -> " " <> toString e) xs <> ")"

class ToSexp a where
  toSexp :: a -> Sexp

instance toSexpVoid :: ToSexp Void where
  toSexp = absurd

instance toSexpUnit :: ToSexp Unit where
  toSexp _ = List Nil

instance toSexpBoolean :: ToSexp Boolean where
  toSexp true  = Atom "true"
  toSexp false = Atom "false"

instance toSexpInt :: ToSexp Int where
  toSexp n = Atom (show n)

instance toSexpNumber :: ToSexp Number where
  toSexp n = Atom (show n)

instance toSexpString :: ToSexp String where
  toSexp = Atom

instance toSexpArray :: (ToSexp a) => ToSexp (Array a) where
  toSexp xs = List (List.fromFoldable (map toSexp xs))

instance toSexpOrdering :: ToSexp Ordering where
  toSexp EQ = Atom "EQ"
  toSexp LT = Atom "LT"
  toSexp GT = Atom "GT"

instance toSexpList :: (ToSexp a) => ToSexp (List a) where
  toSexp xs = List (map toSexp xs)

instance toSexpMaybe :: (ToSexp a) => ToSexp (Maybe a) where
  toSexp (Just x) = List (Atom "Just"    : toSexp x : Nil)
  toSexp Nothing  = Atom "Nothing"

instance toSexpTuple :: (ToSexp a, ToSexp b) => ToSexp (Tuple a b) where
  toSexp (Tuple a b) = List (toSexp a : toSexp b : Nil)

instance toSexpEither :: (ToSexp a, ToSexp b) => ToSexp (Either a b) where
  toSexp (Left x)  = List (Atom "Left"  : toSexp x : Nil)
  toSexp (Right x) = List (Atom "Right" : toSexp x : Nil)

instance toSexpSet :: (ToSexp a) => ToSexp (Set a) where
  toSexp xs = List (map toSexp (Set.toUnfoldable xs))

instance toSexpMap :: (ToSexp k, ToSexp v) => ToSexp (Map k v) where
  toSexp xs = List (Map.toList xs >>= \(Tuple k v) -> toSexp k : toSexp v : Nil)

instance toSexpStrMap :: (ToSexp v) => ToSexp (StrMap v) where
  toSexp xs = List (StrMap.toList xs >>= \(Tuple k v) -> Atom k : toSexp v : Nil)

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
