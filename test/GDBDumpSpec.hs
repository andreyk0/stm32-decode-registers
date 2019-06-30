{-# LANGUAGE NoImplicitPrelude #-}
module GDBDumpSpec (spec) where


import           GDBDump
import           Import
import qualified RIO.Text.Lazy         as LT
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Text.Megaparsec
import           Text.Printf
import           Types


spec :: Spec
spec = do
  describe "parseHex8" $
    prop "to/from hex" $ \i ->
      tryParse parseHex8 (printf "0x%02x" i) `shouldBe` Right i

  describe "parseHex32" $
    prop "to/from hex" $ \i ->
      tryParse parseHex32 (printf "0x%08x" i) `shouldBe` Right i

  describe "parseAddress" $
    it "should parse address" $
      tryParse parseAddress "0x40021000:" `shouldBe` Right (Address 0x40021000)

  describe "findAddress" $ do
    it "should find address" $
      tryParse (findAddress <* takeRest)
        "foo meah0x00000000\n0x40021000:     0x83"
          `shouldBe` Right (Address 0x40021000)

    it "should find address 1" $
      tryParse (findAddress <* takeRest)
        "gdb) x/64xb 0x40021000\n0x40021000:     0x83"
          `shouldBe` Right (Address 0x40021000)

  describe "nextByteOrAddress" $ do
    it "should parse next address" $
      tryParse (nextByteOrAddress <* takeRest)
        "\n0x40021000:     0x83    0x46"
        `shouldBe` Right (Left (Address 0x40021000))

    it "should parse next byte" $
      tryParse (nextByteOrAddress <* takeRest)
        "     0x83    0x46"
        `shouldBe` Right (Right 0x83)

  describe "parseDumpFile" $
    it "should parse" $ do
      res <- parseDumpFile "data/STM32F103xx.dump"
      res `shouldBe` (Address 0x40021000 , testData)


testData :: [Word8]
testData =
  [ 0x83 ,  0x46 ,  0x03 ,  0x03 ,  0x7a ,  0xdc ,  0x1d ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x14 ,  0x00 ,  0x00 ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x1c
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00
  , 0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00 ,  0x00
  ]


tryParse
  :: Parser a
  -> String
  -> Either ParseEB a
tryParse p s =
  runParser p s (LT.pack s)
