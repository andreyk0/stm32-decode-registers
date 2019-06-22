{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module SVDData
  ( stmDeviceModels
  ) where


import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LB
import           Data.FileEmbed
import           Import
import qualified System.FilePath.Posix  as PFP
import           Types



stmDeviceModels :: [DeviceModel]
stmDeviceModels = DeviceModel . fst <$> stmSvdFileData


stmSvdFileData :: [(FilePath, LB.ByteString)]
stmSvdFileData = nextEntry stmTarEntries
  where
    nextEntry Tar.Done        = []
    nextEntry (Tar.Fail e)    = error $ show e

    nextEntry (Tar.Next e@(Tar.entryContent -> (Tar.NormalFile bs _)) es) =
      let (bname, ext) = (PFP.takeBaseName &&& PFP.takeExtension) (Tar.entryPath e)
       in if ext == ".svd"
             then (bname, bs) : nextEntry es
             else nextEntry es

    nextEntry (Tar.Next _ es) = nextEntry es


stmTarEntries :: Tar.Entries Tar.FormatError
stmTarEntries  = Tar.read $ GZ.decompress stmTgz


stmTgz :: LB.ByteString
stmTgz = LB.fromStrict $(embedFile "STMicro.tgz")
