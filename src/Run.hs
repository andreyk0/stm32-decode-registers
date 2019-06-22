{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import           Import
import           SVDData

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  liftIO printTarEntries

