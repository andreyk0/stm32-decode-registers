{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}


module SVDDevice
  ( allRegisterOffsets
  , decodeRegisterData
  ) where


import           Data.Bits
import           Data.SVD.Types
import           Import
import qualified RIO.Map        as Map
import           Types


allRegisterOffsets
  :: Device
  -> Map Address PeripheralRegister
allRegisterOffsets Device{..} = Map.fromList $ (view prAddress &&& id) <$>
  [ PeripheralRegister (regAddress p r) p r
  | p <- devicePeripherals
  , r <- periphRegisters p
  ]
  where
    regAddress p r = Address . fromIntegral $ periphBaseAddress p + regAddressOffset r


-- | Decode register data from GDB hex dump
decodeRegisterData
  :: Map Address PeripheralRegister
  -> (Address, [Word8]) -- ^ Initial offset and data bytes
  -> [PeripheralFieldValue]
decodeRegisterData addr2reg hDump = mconcat $
  decodeFields <$> registerData addr2reg (addrValues hDump)


-- | Address / data pairs
addrValues
  :: (Address, [Word8]) -- ^ Initial offset and data bytes
  -> [(Address, Word8)] -- ^ address/data
addrValues (off, bs) = zip (addrs off) bs
  where
    addrs !a = a : addrs (a + Address 1)


-- | Associates memory data with registers
registerData
  :: Map Address PeripheralRegister
  -> [(Address, Word8)] -- ^ address/data
  -> [(PeripheralRegister, [Word8])]
registerData _ []                    = []
registerData addr2reg ds@((a,_):_) =
  maybe (more 1)
        (\pr -> ( pr
                , snd <$> take (rSzBytes pr) ds)
                  : more (rSzBytes pr)
                )
        (Map.lookup a addr2reg)
  where
    more n = registerData addr2reg (drop n ds)
    rSzBytes pr = pr ^. prRegisterSize `div` 8


decodeFields
  :: (PeripheralRegister, [Word8])
  -> [PeripheralFieldValue]
decodeFields (pr, [w0,w1,w2,w3]) = -- assuming 32b registers
  decodeFieldVal mkPfv regVal <$> pr ^. prRegisterFields
  where
    regVal = fromIntegral w0 +
             fromIntegral w1 `shiftL` 8 +
             fromIntegral w2 `shiftL` 16 +
             fromIntegral w3 `shiftL` 24
    mkPfv = PeripheralFieldValue (pr ^. prPeripheral) (pr ^. prRegister)

decodeFields _                   = []


decodeFieldVal
  :: (Field -> Word32 -> a)
  -> Word32
  -> Field
  -> a
decodeFieldVal mkRes regVal fld = mkRes fld fldVal
  where
    fldMask :: Word32
    fldMask = foldl' setBit zeroBits [0..(fieldBitWidth fld - 1)] `shiftL` fieldBitOffset fld

    fldVal :: Word32
    fldVal = regVal .&. fldMask `shiftR` fieldBitOffset fld
