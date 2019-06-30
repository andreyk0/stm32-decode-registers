{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Data.SVD.Types
import           Import
import           Lens.Micro.TH
import           RIO.Process

newtype DeviceModel = DeviceModel String deriving (Eq,Ord,Show)


-- | Main CLI command
data CLICommand =
    CMDList
  | CMDPrintSVD DeviceModel
  | CMDPrintRegisters DeviceModel
  | CMDDecode DeviceModel FilePath
    -- ^ device and a filepath to a hex memory dump
  deriving (Eq,Ord,Show)


-- | Command line arguments
newtype Options = Options
  { _optVerbose :: Bool
  }

makeLenses ''Options


data App = App
  { _appLogFunc        :: !LogFunc
  , _appProcessContext :: !ProcessContext
  , _appOptions        :: !Options
  }

makeLenses ''App


instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext


newtype Address = Address Word32 deriving (Eq, Ord, Show, Num)


data PeripheralRegister = PeripheralRegister
  { _prAddress    :: !Address
  , _prPeripheral :: !Peripheral
  , _prRegister   :: !Register
  } deriving (Eq, Ord, Show)

makeLenses ''PeripheralRegister

prPeripheralName :: SimpleGetter PeripheralRegister String
prPeripheralName = prPeripheral . to periphName

prRegisterName :: SimpleGetter PeripheralRegister String
prRegisterName = prRegister . to regName

prRegisterDescription :: SimpleGetter PeripheralRegister String
prRegisterDescription = prRegister . to regDescription

prRegisterSize :: SimpleGetter PeripheralRegister Int
prRegisterSize = prRegister . to regSize

prRegisterFields :: SimpleGetter PeripheralRegister [Field]
prRegisterFields = prRegister . to regFields


data PeripheralFieldValue = PeripheralFieldValue
  { _pfvPeripheral :: !Peripheral
  , _pfvRegister   :: !Register
  , _pfvField      :: !Field
  , _pfvValue      :: !Word32
  } deriving (Eq, Ord, Show)

makeLenses ''PeripheralFieldValue

pfvPeripheralName :: SimpleGetter PeripheralFieldValue String
pfvPeripheralName = pfvPeripheral . to periphName

pfvRegisterName :: SimpleGetter PeripheralFieldValue String
pfvRegisterName = pfvRegister . to regName

pfvFieldName :: SimpleGetter PeripheralFieldValue String
pfvFieldName = pfvField . to fieldName
