{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Import
import           Prelude  (putStrLn)
import qualified RIO.List as List
import           SVDData

run
  :: CLICommand
  -> RIO App ()


run CMDList =
  forM_ (List.sort stmDeviceModels) $ \(DeviceModel m) ->
    liftIO $ putStrLn m


{-

    CMDList
  | CMDPrint DeviceModel
  | CMDDecode DeviceModel
-}
