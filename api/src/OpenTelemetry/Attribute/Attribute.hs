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
Module      :  OpenTelemetry.Attribute
Copyright   :  (c) Ian Duncan, 2021
License     :  BSD-3
Description :  Key-value pair metadata used in 'OpenTelemetry.Trace.Span's, 'OpenTelemetry.Trace.Link's, and 'OpenTelemetry.Trace.Event's
Maintainer  :  Ian Duncan
Stability   :  experimental
Portability :  non-portable (GHC extensions)
-}
module OpenTelemetry.Attribute.Attribute (
  Attribute (..),
  IsAttribute (..),
  PrimitiveAttribute (..),
  IsPrimitiveAttribute (..),
) where

import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import qualified Data.List as L
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (lookup, map)


-- | Convert a Haskell value to a 'PrimitiveAttribute' value.
class IsPrimitiveAttribute a where
  toPrimitiveAttribute :: a -> PrimitiveAttribute
  fromPrimitiveAttribute :: PrimitiveAttribute -> Maybe a


{- | An attribute represents user-provided metadata about a span, link, or event.

 Telemetry tools may use this data to support high-cardinality querying, visualization
 in waterfall diagrams, trace sampling decisions, and more.
-}
data Attribute
  = -- | An attribute representing a single primitive value
    AttributeValue PrimitiveAttribute
  | -- | An attribute representing an array of primitive values.
    --
    -- All values in the array MUST be of the same primitive attribute type.
    AttributeArray [PrimitiveAttribute]
  deriving stock (Read, Show, Eq, Ord, Data, Generic)
  deriving anyclass (Hashable)


{- | Create a `TextAttribute` from the string value.

 @since 0.0.2.1
-}
instance IsString PrimitiveAttribute where
  fromString = TextAttribute . fromString


{- | Create a `TextAttribute` from the string value.

 @since 0.0.2.1
-}
instance IsString Attribute where
  fromString = AttributeValue . fromString


data PrimitiveAttribute
  = TextAttribute Text
  | BoolAttribute Bool
  | DoubleAttribute Double
  | IntAttribute Int64
  deriving stock (Read, Show, Eq, Ord, Data, Generic)
  deriving anyclass (Hashable)


{- | Convert a Haskell value to an 'Attribute' value.

 For most values, you can define an instance of 'IsPrimitiveAttribute' and use the default 'toAttribute' implementation:

 @

 data Foo = Foo

 instance IsPrimitiveAttribute Foo where
   toPrimitiveAttribute Foo = TextAttribute "Foo"
 instance IsAttribute foo

 @
-}
class IsAttribute a where
  toAttribute :: a -> Attribute
  default toAttribute :: (IsPrimitiveAttribute a) => a -> Attribute
  toAttribute = AttributeValue . toPrimitiveAttribute
  fromAttribute :: Attribute -> Maybe a
  default fromAttribute :: (IsPrimitiveAttribute a) => Attribute -> Maybe a
  fromAttribute (AttributeValue v) = fromPrimitiveAttribute v
  fromAttribute _ = Nothing


instance IsPrimitiveAttribute PrimitiveAttribute where
  toPrimitiveAttribute = id
  fromPrimitiveAttribute = Just


instance IsAttribute PrimitiveAttribute where
  toAttribute = AttributeValue
  fromAttribute (AttributeValue v) = Just v
  fromAttribute _ = Nothing


instance IsPrimitiveAttribute Text where
  toPrimitiveAttribute = TextAttribute
  fromPrimitiveAttribute (TextAttribute v) = Just v
  fromPrimitiveAttribute _ = Nothing


instance IsAttribute Text


instance IsPrimitiveAttribute Bool where
  toPrimitiveAttribute = BoolAttribute
  fromPrimitiveAttribute (BoolAttribute v) = Just v
  fromPrimitiveAttribute _ = Nothing


instance IsAttribute Bool


instance IsPrimitiveAttribute Double where
  toPrimitiveAttribute = DoubleAttribute
  fromPrimitiveAttribute (DoubleAttribute v) = Just v
  fromPrimitiveAttribute _ = Nothing


instance IsAttribute Double


instance IsPrimitiveAttribute Int64 where
  toPrimitiveAttribute = IntAttribute
  fromPrimitiveAttribute (IntAttribute v) = Just v
  fromPrimitiveAttribute _ = Nothing


instance IsAttribute Int64


instance IsPrimitiveAttribute Int where
  toPrimitiveAttribute = IntAttribute . fromIntegral
  fromPrimitiveAttribute (IntAttribute v) = Just $ fromIntegral v
  fromPrimitiveAttribute _ = Nothing


instance IsAttribute Int


instance IsAttribute Attribute where
  toAttribute = id
  fromAttribute = Just


instance (IsPrimitiveAttribute a) => IsAttribute [a] where
  toAttribute = AttributeArray . L.map toPrimitiveAttribute
  fromAttribute (AttributeArray arr) = traverse fromPrimitiveAttribute arr
  fromAttribute _ = Nothing
