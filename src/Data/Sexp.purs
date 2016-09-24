-- | S-expressions.
-- |
-- | The textual representations is simple. The following rules apply:
-- |
-- | - Whitespace is ignored outside atoms, and matches **only** `[ \t\r\n]+`.
-- | - Only backslashes and double quotes can be escaped inside atoms; anything
-- |   else must appear verbatim. Newlines and any other character are allowed
-- |   inside atoms.
-- | - Text following the first S-expression is ignored.
module Data.Sexp
( Sexp(..)
, toString, fromString
, class ToSexp, toSexp
, class FromSexp, fromSexp
, class AsSexp
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
import Global (readFloat)
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

-- | S-expression.
data Sexp = Atom String | List (List Sexp)

derive instance eqSexp :: Eq Sexp
derive instance ordSexp :: Ord Sexp
derive instance genericSexp :: Generic Sexp

instance arbitrarySexp :: Arbitrary Sexp where
  arbitrary = Gen.sized arbitrary'
    where
    arbitrary' 0 = Atom <$> arbitrary
    arbitrary' n = List <$> Gen.listOf (n / 2) (arbitrary' (n / 2))

-- | Compute the textual representation of an S-expression.
toString :: Sexp -> String
toString (Atom s)  = "\"" <> _escape s <> "\""
toString (List Nil) = "()"
toString (List (x : xs)) = "(" <> toString x <> foldMap (\e -> " " <> toString e) xs <> ")"

foreign import _escape :: String -> String

-- | Parse the textual representation of an S-expression.
fromString :: String -> Maybe Sexp
fromString = _fromString { nothing: Nothing
                         , just:    Just
                         , nil:     Nil
                         , cons:    Cons
                         , reverse: List.reverse
                         , atom:    Atom
                         , list:    List
                         }

foreign import _fromString
  :: { nothing :: forall a. Maybe a
     , just    :: forall a. a -> Maybe a
     , nil     :: forall a. List a
     , cons    :: forall a. a -> List a -> List a
     , reverse :: forall a. List a -> List a
     , atom    :: String -> Sexp
     , list    :: List Sexp -> Sexp
     }
  -> String
  -> Maybe Sexp

-- | Things that can be converted into S-expressions.
class ToSexp a where
  toSexp :: a -> Sexp

instance toSexpSexp :: ToSexp Sexp where
  toSexp = id

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

instance fromSexpSexp :: FromSexp Sexp where
  fromSexp = Just

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

instance fromSexpNumber :: FromSexp Number where
  fromSexp (Atom x) = Just $ readFloat x
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
  fromSexp (List (Atom "SChar"    : c : Nil))    = SChar    <$> fromSexp c
  fromSexp (List (Atom "SString"  : s : Nil))    = SString  <$> fromSexp s
  fromSexp (List (Atom "SInt"     : i : Nil))    = SInt     <$> fromSexp i
  fromSexp (List (Atom "SBoolean" : b : Nil))    = SBoolean <$> fromSexp b
  fromSexp (List (Atom "SNumber"  : n : Nil))    = SNumber  <$> fromSexp n
  fromSexp (List (Atom "SRecord"  : r))          =
    let go acc Nil = Just (List.reverse acc)
        go acc (Atom k : v : tail) = do
          v' <- fromSexp v
          go ({recLabel: k, recValue: const v'} : acc) tail
        go acc _ = Nothing
    in SRecord <<< List.toUnfoldable <$> go Nil r
  fromSexp (List (Atom "SProd"    : Atom c : r)) =
    SProd c <<< List.toUnfoldable <<< map const <$> traverse fromSexp r
  fromSexp _ = Nothing

-- | Instances must satisfy the following law:
-- |
-- | - Losslessness: `fromSexp (toSexp x) = Just x`
class (ToSexp a, FromSexp a) <= AsSexp a

instance asSexpSexp :: AsSexp Sexp
instance asSexpVoid :: AsSexp Void
instance asSexpUnit :: AsSexp Unit
instance asSexpBoolean :: AsSexp Boolean
instance asSexpChar :: AsSexp Char
instance asSexpInt :: AsSexp Int
instance asSexpNumber :: AsSexp Number
instance asSexpString :: AsSexp String
instance asSexpArray :: (AsSexp a) => AsSexp (Array a)
instance asSexpOrdering :: AsSexp Ordering
instance asSexpList :: (AsSexp a) => AsSexp (List a)
instance asSexpMaybe :: (AsSexp a) => AsSexp (Maybe a)
instance asSexpTuple :: (AsSexp a, AsSexp b) => AsSexp (Tuple a b)
instance asSexpEither :: (AsSexp a, AsSexp b) => AsSexp (Either a b)
instance asSexpGenericSpine :: AsSexp GenericSpine

-- | Convert anything to an S-expression.
gToSexp :: forall a. (Generic a) => a -> Sexp
gToSexp = toSpine >>> toSexp

-- | Convert an S-expression to anything.
gFromSexp :: forall a. (Generic a) => Sexp -> Maybe a
gFromSexp = fromSexp >=> fromSpine
