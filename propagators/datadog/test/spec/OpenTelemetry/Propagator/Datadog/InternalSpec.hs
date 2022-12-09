{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module OpenTelemetry.Propagator.Datadog.InternalSpec where

import OpenTelemetry.Propagator.Datadog.Internal
import qualified Old

import Test.Hspec
import Test.QuickCheck

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
            HexString (Old.newTraceIdFromHeader x')

  context "newSpanIdFromHeader" $ do
    it "is equal to the old implementation" $
      property $ \x -> do
        let x' = BC.pack $ show (x :: Word64)
        HexString (newSpanIdFromHeader x')
          `shouldBe`
            HexString (Old.newSpanIdFromHeader x')

  context "newHeaderFromTraceId" $ do
    it "is equal to the old implementation" $
      property $ \(x1, x2, x3, x4, x5, x6, x7, x8, x9, (x10, x11, x12, x13, x14, x15, x16)) -> do
        let x' = SB.pack [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16]
        newHeaderFromTraceId x'
          `shouldBe`
            Old.newHeaderFromTraceId x'

  context "newHeaderFromSpanId" $ do
    it "is equal to the old implementation" $
      property $ \(x1, x2, x3, x4, x5, x6, x7, x8) -> do
        let x' = SB.pack [x1, x2, x3, x4, x5, x6, x7, x8]
        newHeaderFromSpanId x'
          `shouldBe`
            Old.newHeaderFromSpanId x'

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

newtype HexString = HexString ShortByteString deriving Eq

instance Show HexString where
  show (HexString s) = simpleHex $ SB.fromShort s
