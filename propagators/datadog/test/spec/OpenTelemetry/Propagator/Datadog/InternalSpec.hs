{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module OpenTelemetry.Propagator.Datadog.InternalSpec where

import OpenTelemetry.Propagator.Datadog.Internal

import Test.Hspec
import Test.QuickCheck

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SB
import qualified Data.Char             as C
import           Data.Word             (Word64)
import           Hexdump               (simpleHex)

spec :: Spec
spec = do
  context "newTraceIdFromHeader" $ do
    it "is equal to the old implementation" $
      property $ \x -> do
        let x' = BC.pack $ show (x :: Word64)
        HexString (newTraceIdFromHeader x')
          `shouldBe`
            HexString (SB.toShort $ fillLeadingZeros 16 $ convertWord64ToBinaryByteString $ read $ BC.unpack x')

  context "newSpanIdFromHeader" $ do
    it "is equal to the old implementation" $
      property $ \x -> do
        let x' = BC.pack $ show (x :: Word64)
        HexString (newSpanIdFromHeader x')
          `shouldBe`
            HexString (SB.toShort $ fillLeadingZeros 8 $ convertWord64ToBinaryByteString $ read $ BC.unpack x')

  context "newHeaderFromTraceId" $ do
    it "is equal to the old implementation" $
      property $ \(x1, x2, x3, x4, x5, x6, x7, x8, x9, (x10, x11, x12, x13, x14, x15, x16)) -> do
        let x' = SB.pack [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16]
        newHeaderFromTraceId x'
          `shouldBe`
            BC.pack (show $ convertBinaryByteStringToWord64 $ SB.fromShort x')

  context "newHeaderFromSpanId" $ do
    it "is equal to the old implementation" $
      property $ \(x1, x2, x3, x4, x5, x6, x7, x8) -> do
        let x' = SB.pack [x1, x2, x3, x4, x5, x6, x7, x8]
        newHeaderFromSpanId x'
          `shouldBe`
            BC.pack (show $ convertBinaryByteStringToWord64 $ SB.fromShort x')

  context "readWord64BS" $ do
    it "can get back a value" $
      property $ \x ->
        readWord64BS (BC.pack $ show x) `shouldBe` x

  context "showWord64BS" $ do
    it "can show back a value" $
      property $ \x ->
        showWord64BS x `shouldBe` BC.pack (show x)

  context "asciiWord8ToWord8" $ do
    it "returns 0 for '0'" $
      asciiWord8ToWord8 (fromIntegral $ C.ord '0') `shouldBe` 0

  context "word8ToAsciiWord8" $ do
    it "returns '0' for 0" $
      word8ToAsciiWord8 0 `shouldBe` fromIntegral (C.ord '0')

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

newtype HexString = HexString ShortByteString deriving Eq

instance Show HexString where
  show (HexString s) = simpleHex $ SB.fromShort s
