{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import OpenTelemetry.Propagator.Datadog.Internal
import OpenTelemetry.Trace.Id                    (TraceId)

import qualified Criterion.Main as C

import           Control.DeepSeq       (NFData)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as SB
import           Data.Word             (Word64)

main :: IO ()
main =
  C.defaultMain
    [ C.bgroup "newTraceIdFromHeader"
        [ C.bench "new" $ C.nf newTraceIdFromHeader "1"
        , C.bench "old" $ C.nf (fillLeadingZeros 16 . convertWord64ToBinaryByteString . read) "1"
        ]
    , C.bgroup "newHeaderFromTraceId" $
        let value = SB.pack [0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 10, 11, 12, 13, 14, 15]
        in
          [ C.bench "new" $ C.nf newHeaderFromTraceId value
          , C.bench "old" $ C.nf (BC.pack . show . convertBinaryByteStringToWord64 . SB.fromShort) value
          ]
    ]

instance NFData TraceId

convertWord64ToBinaryByteString :: Word64 -> ByteString
convertWord64ToBinaryByteString =
  B.pack . toWord8s []
  where
    toWord8s acc 0 = acc
    toWord8s acc n =
      let (p, q) = n `divMod` (2 ^ (8 :: Int))
      in toWord8s (fromIntegral q : acc) p

fillLeadingZeros :: Word -> ByteString -> ByteString
fillLeadingZeros len bs = B.replicate (fromIntegral len - B.length bs) 0 <> bs

convertBinaryByteStringToWord64 :: ByteString -> Word64
convertBinaryByteStringToWord64 = B.foldl (\acc b -> (2 ^ (8 :: Int)) * acc + fromIntegral b) 0 -- GHC.Prim.indexWord8ArrayAsWord64# とか駆使すると早くなりそう
