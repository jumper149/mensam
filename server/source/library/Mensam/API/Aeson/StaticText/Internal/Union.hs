{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

-- | This module is heavily inspired by servant's Servant.Api.Uverb.Union.
module Mensam.API.Aeson.StaticText.Internal.Union where

import Data.Kind
import Data.SOP.BasicFunctors (I)
import Data.SOP.NS
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits

type Union :: [Type] -> Type
type Union = NS I

-- * Stuff stolen from 'Data.WorldPeace" but for generics-sop

-- (this could to go sop-core, except it's probably too specialized to the servant use-case.)

type IsMember :: u -> [u] -> Constraint
type IsMember (a :: u) (as :: [u]) = (Unique as, CheckElemIsMember a as, UElem a as)

type UElem :: k -> [k] -> Constraint
class UElem x xs where
  inject :: f x -> NS f xs
  eject :: NS f xs -> Maybe (f x)

instance {-# OVERLAPPING #-} UElem x (x ': xs) where
  inject = Z
  eject (Z x) = Just x
  eject _ = Nothing

instance {-# OVERLAPPING #-} UElem x xs => UElem x (x' ': xs) where
  inject = S . inject
  eject (Z _) = Nothing
  eject (S ns) = eject ns

-- | Check whether @a@ is in given type-level list.
-- This will throw a nice error if the element is not in the list.
type CheckElemIsMember :: k -> [k] -> Constraint
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
  CheckElemIsMember a as =
    If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))

type NoElementError :: k -> [k] -> ErrorMessage
type NoElementError (r :: k) (rs :: [k]) =
  'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r

type DuplicateElementError :: [k] -> ErrorMessage
type DuplicateElementError (rs :: [k]) =
  'Text "Duplicate element in list:"
    ':$$: 'Text "    " ':<>: 'ShowType rs

type Elem :: k -> [k] -> Bool
type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs
  Elem _ '[] = 'False

-- | Check whether all values in a type-level list are distinct.
-- This will throw a nice error if there are any duplicate elements in the list.
type Unique :: [k] -> Constraint
type family Unique xs :: Constraint where
  Unique xs = If (Nubbed xs == 'True) (() :: Constraint) (TypeError (DuplicateElementError xs))

type Nubbed :: [k] -> Bool
type family Nubbed xs :: Bool where
  Nubbed '[] = 'True
  Nubbed (x ': xs) = If (Elem x xs) 'False (Nubbed xs)

type Map :: (a -> b) -> [a] -> [b]
type family Map f xs where
  Map _ '[] = '[]
  Map f (x : xs) = f x : Map f xs
