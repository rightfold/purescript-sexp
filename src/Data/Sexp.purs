-- | S-expressions.
module Data.Sexp
( Sexp(..)
, toString
, class ToSexp, toSexp
, class FromSexp, fromSexp
, gToSexp, gFromSexp
) where

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic (fromSpine, class Generic, GenericSpine(..), toSpine)
import Data.Int as Int
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
import Data.Traversable (traverse)
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

-- | Things that can be converted into S-expressions.
class ToSexp a where
  toSexp :: a -> Sexp

instance toSexpVoid :: ToSexp Void where
  toSexp = absurd

instance toSexpUnit :: ToSexp Unit where
  toSexp _ = List Nil

instance toSexpBoolean :: ToSexp Boolean where
  toSexp true  = Atom "true"
  toSexp false = Atom "false"

instance toSexpChar :: ToSexp Char where
  toSexp c = Atom (String.singleton c)

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
  toSexp SUnit        = Atom "SUnit"
  toSexp (SArray xs)  = List (Atom "SArray"   : List.fromFoldable (map (\thk -> toSexp (thk unit)) xs))
  toSexp (SChar c)    = List (Atom "SChar"    : toSexp c : Nil)
  toSexp (SString s)  = List (Atom "SString"  : toSexp s : Nil)
  toSexp (SInt i)     = List (Atom "SInt"     : toSexp i : Nil)
  toSexp (SBoolean b) = List (Atom "SBoolean" : toSexp b : Nil)
  toSexp (SNumber n)  = List (Atom "SNumber"  : toSexp n : Nil)
  toSexp (SRecord r)  = List (Atom "SRecord"  : List.fromFoldable (r >>= \{recLabel, recValue} -> [Atom recLabel, toSexp (recValue unit)]))
  toSexp (SProd c xs) = List (Atom "SProd"    : Atom c : List.fromFoldable (map (\thk -> toSexp (thk unit)) xs))

-- | Things that S-expressions can be converted into.
class FromSexp a where
  fromSexp :: Sexp -> Maybe a

instance fromSexpVoid :: FromSexp Void where
  fromSexp _ = Nothing

instance fromSexpUnit :: FromSexp Unit where
  fromSexp (List Nil) = Just unit
  fromSexp _ = Nothing

instance fromSexpBoolean :: FromSexp Boolean where
  fromSexp (Atom "true")  = Just true
  fromSexp (Atom "false") = Just false
  fromSexp _ = Nothing

instance fromSexpChar :: FromSexp Char where
  fromSexp (Atom x) = String.toChar x
  fromSexp _ = Nothing

instance fromSexpInt :: FromSexp Int where
  fromSexp (Atom x) = Int.fromString x
  fromSexp _ = Nothing

instance fromSexpString :: FromSexp String where
  fromSexp (Atom x) = Just x
  fromSexp _ = Nothing

instance fromSexpArray :: (FromSexp a) => FromSexp (Array a) where
  fromSexp (List xs) = traverse fromSexp (List.toUnfoldable xs)
  fromSexp _ = Nothing

instance fromSexpOrdering :: FromSexp Ordering where
  fromSexp (Atom "EQ") = Just EQ
  fromSexp (Atom "LT") = Just LT
  fromSexp (Atom "GT") = Just GT
  fromSexp _ = Nothing

instance fromSexpList :: (FromSexp a) => FromSexp (List a) where
  fromSexp (List xs) = traverse fromSexp xs
  fromSexp _ = Nothing

instance fromSexpMaybe :: (FromSexp a) => FromSexp (Maybe a) where
  fromSexp (List (Atom "Just" : x : Nil)) = Just <$> fromSexp x
  fromSexp (Atom "Nothing") = Just Nothing
  fromSexp _ = Nothing

instance fromSexpTuple :: (FromSexp a, FromSexp b) => FromSexp (Tuple a b) where
  fromSexp (List (a : b : Nil)) = Tuple <$> fromSexp a <*> fromSexp b
  fromSexp _ = Nothing

instance fromSexpEither :: (FromSexp a, FromSexp b) => FromSexp (Either a b) where
  fromSexp (List (Atom "Left"  : x : Nil)) = Left  <$> fromSexp x
  fromSexp (List (Atom "Right" : x : Nil)) = Right <$> fromSexp x
  fromSexp _ = Nothing

instance fromSexpGenericSpine :: FromSexp GenericSpine where
  fromSexp       (Atom "SUnit")                  = Just SUnit
  fromSexp (List (Atom "SArray"   : xs))         =
    SArray <<< List.toUnfoldable <<< map const <$> traverse fromSexp xs
  fromSexp (List (Atom "SChar"    : c : Nil))    = SChar <$> fromSexp c
  fromSexp (List (Atom "SString"  : s : Nil))    = fromSexp s
  fromSexp (List (Atom "SInt"     : i : Nil))    = fromSexp i
  fromSexp (List (Atom "SBoolean" : b : Nil))    = fromSexp b
  fromSexp (List (Atom "SNumber"  : n : Nil))    = fromSexp n
  fromSexp (List (Atom "SRecord"  : r))          =
    let go acc Nil = Just acc
        go acc (Atom k : v : tail) = do
          v' <- fromSexp v
          go ({recLabel: k, recValue: \_ -> v'} : acc) tail
        go acc _ = Nothing
    in SRecord <<< List.toUnfoldable <$> go Nil r
  fromSexp (List (Atom "SProd"    : Atom c : r)) =
    SProd c <<< List.toUnfoldable <<< map const <$> traverse fromSexp r
  fromSexp _ = Nothing

-- | Convert anything to an S-expression.
gToSexp :: forall a. (Generic a) => a -> Sexp
gToSexp = toSpine >>> toSexp

-- | Convert an S-expression to anything.
gFromSexp :: forall a. (Generic a) => Sexp -> Maybe a
gFromSexp = fromSexp >=> fromSpine
