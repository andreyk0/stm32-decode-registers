{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import qualified Data.SVD.Pretty              as SVD
import qualified Data.Text.IO                 as IO
import           Import
import           Prelude                      (print, putStrLn)
import qualified RIO.List                     as List
import           SVDData
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Types

run
  :: CLICommand
  -> RIO App ()


run CMDList =
  forM_ (List.sort stmDeviceModels) $ \(DeviceModel m) ->
    say' m


run (CMDPrint m) = do
  d <- liftIO $ lookupStmDevice m
  say' $ SVD.ppDevice d
