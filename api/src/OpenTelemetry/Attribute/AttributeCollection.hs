{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{- |
Module      :  OpenTelemetry.AttributeCollection
Copyright   :  (c) Ian Duncan, 2021
License     :  BSD-3
Description :  Key-value pair metadata used in 'OpenTelemetry.Trace.Span's, 'OpenTelemetry.Trace.Link's, and 'OpenTelemetry.Trace.Event's
Maintainer  :  Ian Duncan
Stability   :  experimental
Portability :  non-portable (GHC extensions)

An Attribute is a key-value pair, which MUST have the following properties:

- The attribute key MUST be a non-@null@ and non-empty string.
- The attribute value is either:

    - A primitive type: string, boolean, double precision floating point (IEEE 754-1985) or signed 64 bit integer.
    - An array of primitive type values. The array MUST be homogeneous, i.e., it MUST NOT contain values of different types. For protocols that do not natively support array values such values SHOULD be represented as JSON strings.

Attribute values expressing a numerical value of zero, an empty string, or an empty array are considered meaningful and MUST be stored and passed on to processors \/ exporters.

Specification: https://opentelemetry.io/docs/specs/otel/common/
-}
module OpenTelemetry.Attribute.AttributeCollection (
  AttributeCollection,
  emptyAttributes,
  addAttribute,
  addAttributes,
  lookupAttribute,
  attributes,
  count,

  -- * Attribute limits
  AttributeLimits (..),
  defaultAttributeLimits,

  -- * Unsafe utilities
  unsafeAttributesFromListIgnoringLimits,
  unsafeMergeAttributesIgnoringLimits,
) where

import Data.Data (Data)
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import OpenTelemetry.Attribute.Attribute (Attribute (AttributeArray, AttributeValue), IsAttribute (fromAttribute, toAttribute), PrimitiveAttribute (TextAttribute))
import OpenTelemetry.Attribute.Attributes (Attributes)
import qualified OpenTelemetry.Attribute.Attributes as A
import OpenTelemetry.Attribute.Key (Key)
import Prelude hiding (lookup)


{- | Default attribute limits used in the global attribute limit configuration if no environment variables are set.

Values:

- 'attributeCountLimit': @Just 128@
- 'attributeLengthLimit': Infinity or @Nothing@
-}
defaultAttributeLimits :: AttributeLimits
defaultAttributeLimits =
  AttributeLimits
    { attributeCountLimit = Just 128
    , attributeLengthLimit = Nothing
    }


data AttributeCollection = AttributeCollection
  { attributes :: !Attributes
  , attributesCount :: {-# UNPACK #-} !Int
  , attributesDropped :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq)


instance Default AttributeCollection where
  def = emptyAttributes


emptyAttributes :: AttributeCollection
emptyAttributes = AttributeCollection mempty 0 0


addAttribute :: (IsAttribute a) => AttributeLimits -> AttributeCollection -> Key a -> a -> AttributeCollection
addAttribute AttributeLimits {..} AttributeCollection {..} k !v = case attributeCountLimit of
  Nothing -> AttributeCollection newAttrs newCount attributesDropped
  Just limit_ ->
    if newCount > limit_
      then AttributeCollection attributes attributesCount (attributesDropped + 1)
      else AttributeCollection newAttrs newCount attributesDropped
  where
    newAttrs = A.insert k (maybe id limitLengths attributeCountLimit v) attributes
    newCount = A.size newAttrs
{-# INLINE addAttribute #-}


addAttributes :: AttributeLimits -> AttributeCollection -> Attributes -> AttributeCollection
addAttributes AttributeLimits {..} AttributeCollection {..} attrs = case attributeCountLimit of
  Nothing -> AttributeCollection newAttrs newCount attributesDropped
  Just limit_ ->
    if newCount > limit_
      then AttributeCollection attributes attributesCount (attributesDropped + A.size attrs)
      else AttributeCollection newAttrs newCount attributesDropped
  where
    newAttrs = A.union attributes attrs
    newCount = A.size newAttrs
{-# INLINE addAttributes #-}


limitPrimAttr :: Int -> PrimitiveAttribute -> PrimitiveAttribute
limitPrimAttr limit (TextAttribute t) = TextAttribute (T.take limit t)
limitPrimAttr _ attr = attr


limitLengths :: IsAttribute a => Int -> a -> a
limitLengths limit a =
  fromMaybe a $
    fromAttribute $
      case toAttribute a of
        AttributeValue val -> AttributeValue $ limitPrimAttr limit val
        AttributeArray arr -> AttributeArray $ fmap (limitPrimAttr limit) arr


count :: AttributeCollection -> Int
count = attributesCount


lookupAttribute :: AttributeCollection -> Key Attribute -> Maybe Attribute
lookupAttribute AttributeCollection {..} k = A.lookupAttribute k attributes


{- | It is possible when adding attributes that a programming error might cause too many
 attributes to be added to an event. Thus, 'AttributeCollection' use the limits set here as a safeguard
 against excessive memory consumption.
-}
data AttributeLimits = AttributeLimits
  { attributeCountLimit :: Maybe Int
  -- ^ The number of unique attributes that may be added to an 'AttributeCollection' structure before they are attributesDropped.
  , attributeLengthLimit :: Maybe Int
  -- ^ The maximum length of string attributes that may be set. Longer-length string values will be truncated to the
  -- specified amount.
  }
  deriving stock (Read, Show, Eq, Ord, Data, Generic)
  deriving anyclass (Hashable)


instance Default AttributeLimits where
  def = defaultAttributeLimits


unsafeMergeAttributesIgnoringLimits :: AttributeCollection -> AttributeCollection -> AttributeCollection
unsafeMergeAttributesIgnoringLimits (AttributeCollection l lc ld) (AttributeCollection r rc rd) = AttributeCollection (l <> r) (lc + rc) (ld + rd)


unsafeAttributesFromListIgnoringLimits :: IsAttribute a => [(Key a, a)] -> AttributeCollection
unsafeAttributesFromListIgnoringLimits l = AttributeCollection hm c 0
  where
    hm = A.fromList l
    c = A.size hm
