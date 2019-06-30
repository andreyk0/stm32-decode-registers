{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


{- | Parses GDB memory hex dump
     E.g.
     0x40021000:     0x83    0x46    0x03    0x03    0x7a    0xdc    0x1d    0x00
-}
module GDBDump
  ( -- ** Types
    Parser
  , ParseEB

  -- ** Parse HEX dump file
  , parseDumpFile

  -- ** Test subparsers
  , findAddress
  , nextByteOrAddress
  , parseAddress
  , parseHex32
  , parseHex8
  ) where

import           Control.Monad
import           Control.Monad.Combinators
import           Data.Bits                 (shiftL)
import           Data.Char
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Text.Lazy.IO         as TIO
import           Import                    hiding (many, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Types


type Parser = Parsec Void LText
type ParseEB = ParseErrorBundle LText Void


-- | Parses GDB file dump assuming it represents a contiguous block of memory
parseDumpFile
  :: ( MonadIO m
     , MonadThrow m
     )
  => FilePath
  -> m (Address, [Word8]) -- ^ Initial offset and data bytes
parseDumpFile fPath = do
  fData <- liftIO $ TIO.readFile fPath
  either throwM pure (runParser parseHexFile fPath fData)


-- Parse 0x00
parseHex8 :: Parser Word8
parseHex8 = do
  void (chunk  "0x" <?> "hex word8 0x")
  h <- hexDigitChar <?> "hex word8 h"
  l <- hexDigitChar <?> "hex word8 l"
  pure . fromIntegral $ digitToInt h `shiftL` 4 + digitToInt l


parseHex32 :: Parser Word32
parseHex32 = do
  void (chunk  "0x" <?> "hex word32 0x")
  d0 <- hexDigitChar <?> "hex word32 d0"
  d1 <- hexDigitChar <?> "hex word32 d1"
  d2 <- hexDigitChar <?> "hex word32 d2"
  d3 <- hexDigitChar <?> "hex word32 d3"
  d4 <- hexDigitChar <?> "hex word32 d4"
  d5 <- hexDigitChar <?> "hex word32 d5"
  d6 <- hexDigitChar <?> "hex word32 d6"
  d7 <- hexDigitChar <?> "hex word32 d7"

  pure (mkNum [d0,d1,d2,d3,d4,d5,d6,d7]) <?> "hex word32"
  where
    mkNum    = foldl' step 0
    step a c = a * 16 + fromIntegral (digitToInt c)


parseAddress :: Parser Address
parseAddress = do
  w <- parseHex32
  void (char ':' <?> "address ':'")
  pure $ Address w


findAddress :: Parser Address
findAddress =
 skipManyTill (anySingle <?> "any char") (try parseAddress)


nextByteOrAddress :: Parser (Either Address Word8)
nextByteOrAddress = do
  space
  (Left <$> try parseAddress <|> Right <$> try parseHex8) <?> "address or byte"


parseBytesCheckOffset
  :: Address -- ^ current parsed address
  -> Address -- ^ bytes since last parsed offset
  -> ([Word8] -> [Word8])
  -> Parser [Word8]
parseBytesCheckOffset off n fAcc = do
  boa <- optional (try nextByteOrAddress) <?> "next byte or address"
  case boa
    of Nothing -> pure (fAcc [])
       Just (Right b) -> parseBytesCheckOffset off (n + Address 1) (fAcc . (b:))
       Just (Left a)  ->
         if a == off + n
           then parseBytesCheckOffset a (Address 0) fAcc
           else unexpected . Label $ 'E' :| ("xpected contiguous memory dump, got " <> show a)


parseHexFile :: Parser (Address, [Word8])
parseHexFile = do
  off <- findAddress <?> "find address offset"
  dat <- parseBytesCheckOffset off (Address 0) id
  pure (off, dat)
