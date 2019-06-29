{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module GDBDump
  ( parseDumpFile
  ) where

import qualified Data.SVD.Pretty              as SVD
import qualified Data.Text.IO                 as IO
import           Import
import           Prelude                      (print, putStrLn)
import qualified RIO.List                     as List
import           SVDData
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Types


parseDumpFile
  :: FilePath
  -> IO ()
parseDumpFile fPath = do

  pure ()
