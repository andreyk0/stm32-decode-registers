{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           Data.Word
import           RIO
import qualified RIO.Map     as Map
import           RIO.Process


newtype MemMap = MemMap
  { mmRCC :: Word32
  } deriving (Eq, Ord, Show)


memMaps :: Map String MemMap
memMaps = Map.fromList
  [ ( "stm32f103"
    , MemMap
      { mmRCC = 0x40021000
      }
    )
  ]


-- | Command line arguments
data Options = Options
  { optVerbose :: !Bool
  , optMemMap  :: !MemMap
  }


data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
