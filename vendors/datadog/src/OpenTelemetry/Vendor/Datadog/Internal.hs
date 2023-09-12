module OpenTelemetry.Vendor.Datadog.Internal (indexByteArrayNbo) where

import Data.Bits (Bits (shift))
import Data.Primitive.ByteArray (ByteArray (), indexByteArray)
import Data.Word (Word64, Word8)


-- | Read 'ByteArray' to 'Word64' with network-byte-order.
indexByteArrayNbo ::
  ByteArray ->
  -- | Offset in 'Word64'-size unit
  Int ->
  Word64
indexByteArrayNbo ba offset =
  loop 0 0
  where
    loop 8 acc = acc
    loop n acc = loop (n + 1) $ shift acc 8 + word8ToWord64 (indexByteArray ba $ 8 * offset + n)


word8ToWord64 :: Word8 -> Word64
word8ToWord64 = fromIntegral
