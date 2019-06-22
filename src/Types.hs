{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Word
import           Lens.Micro
import           Lens.Micro.TH
import           RIO
import qualified RIO.Map       as Map
import           RIO.Process


newtype DeviceModel = DeviceModel String deriving (Eq,Ord,Show)


-- | Main CLI command
data CLICommand =
    CMDList
  | CMDPrint DeviceModel
  | CMDDecode DeviceModel
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
