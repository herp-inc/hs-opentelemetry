module OpenTelemetry.Attribute (
  AttributeCollection,
  emptyAttributes,
  addAttribute,
  addAttributes,
  lookupAttribute,
  Attribute (..),
  IsAttribute (..),
  PrimitiveAttribute (..),
  IsPrimitiveAttribute (..),
  Key (..),
  Attributes,

  -- * Attribute limits
  AttributeLimits (..),
  defaultAttributeLimits,

  -- * Unsafe utilities
  unsafeAttributesFromListIgnoringLimits,
  unsafeMergeAttributesIgnoringLimits,
) where

import OpenTelemetry.Attribute.Attribute
import OpenTelemetry.Attribute.AttributeCollection
import OpenTelemetry.Attribute.Attributes (Attributes)
import OpenTelemetry.Attribute.Key

