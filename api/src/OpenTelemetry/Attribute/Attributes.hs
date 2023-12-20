{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  OpenTelemetry.AttributeCollection
Copyright   :  (c) Kazuki Okamoto (岡本和樹), 2023
License     :  BSD-3
Description :  Key-value pair metadata used in 'OpenTelemetry.Trace.Span's, 'OpenTelemetry.Trace.Link's, and 'OpenTelemetry.Trace.Event's
Maintainer  :  Kazuki Okamoto (岡本和樹)
Stability   :  experimental
Portability :  non-portable (GHC extensions)
-}
module OpenTelemetry.Attribute.Attributes (
  Attributes (..),
  empty,
  fromList,
  toList,
  insert,
  union,
  unions,
  lookup,
  lookupAttribute,
  size,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.Default.Class (Default (def))
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified GHC.Exts as E
import GHC.Generics (Generic)
import OpenTelemetry.Attribute.Attribute (
  Attribute,
  IsAttribute (..),
 )
import OpenTelemetry.Attribute.Key (
  Key (Key),
 )
import Prelude hiding (lookup, map)


newtype Attributes = Attributes
  {contents :: H.HashMap Text Attribute}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (Hashable)


instance Default Attributes where
  def = mempty


instance E.IsList Attributes where
  type Item Attributes = (Key Attribute, Attribute)
  fromList = fromList
  toList = toList


fromList :: IsAttribute a => [(Key a, a)] -> Attributes
fromList = Attributes . H.fromList . fmap (\(Key k, v) -> (k, toAttribute v))


toList :: Attributes -> [(Key Attribute, Attribute)]
toList = fmap (first Key) . H.toList . contents


empty :: Attributes
empty = mempty


lift :: (H.HashMap Text Attribute -> c) -> Attributes -> c
lift f = f . contents


lift2 :: (H.HashMap Text Attribute -> H.HashMap Text Attribute -> c) -> Attributes -> Attributes -> c
lift2 f a b = f (contents a) (contents b)


map :: (H.HashMap Text Attribute -> H.HashMap Text Attribute) -> Attributes -> Attributes
map f = lift $ Attributes . f


insert :: (IsAttribute a) => Key a -> a -> Attributes -> Attributes
insert (Key !k) !v =
  map $ H.insert k (toAttribute v)


union :: Attributes -> Attributes -> Attributes
union a b = Attributes $ lift2 H.union a b


unions :: [Attributes] -> Attributes
unions = Attributes . H.unions . fmap contents


lookup :: IsAttribute a => Key a -> Attributes -> Maybe a
lookup (Key k) Attributes {..} = H.lookup k contents >>= fromAttribute


lookupAttribute :: Key Attribute -> Attributes -> Maybe Attribute
lookupAttribute (Key k) Attributes {..} = H.lookup k contents


size :: Attributes -> Int
size = lift H.size
