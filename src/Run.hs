{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Import
import           Prelude (print)
import           SVDData

run
  :: CLICommand
  -> RIO App ()


run CMDList =
  forM_ stmDeviceModels (liftIO . print)


{-

    CMDList
  | CMDPrint DeviceModel
  | CMDDecode DeviceModel
-}
