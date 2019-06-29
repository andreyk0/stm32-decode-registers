{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module SVDDevice
  ( allRegisterOffsets
  ) where


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
