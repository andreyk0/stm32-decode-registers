{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module Run (run) where


import qualified Data.SVD.Pretty                       as SVD
import           Data.SVD.Types
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Import
import           Pretty
import qualified RIO.List                              as List
import qualified RIO.Map                               as Map
import           SVDData
import           SVDDevice
import           Types


run
  :: CLICommand
  -> RIO App ()


run CMDList =
  forM_ (List.sort stmDeviceModels)
        (say' @String . coerce)


run (CMDPrintSVD dModel) =
  liftIO (lookupStmDevice dModel) >>=
    say' . SVD.ppDevice


run (CMDPrintRegisters dModel) = do
  d@Device{..} <- liftIO $ lookupStmDevice dModel
  let addr2reg = List.sort $ snd <$> Map.toList (allRegisterOffsets d)
  liftIO $ putDoc $ ppPeripheralRegisters addr2reg


run (CMDDecode dModel hexFile) = do
  d <- liftIO $ lookupStmDevice dModel
  say' $ SVD.ppDevice d
