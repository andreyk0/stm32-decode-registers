{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module SVDData
  ( stmDeviceModels
  , lookupStmDevice
  ) where


import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Compression.GZip          as GZ
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Lazy.UTF8       as UTF8
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.SVD                        as SVD
import           Import
import qualified System.FilePath.Posix           as PFP
import qualified Text.XML.HXT.Arrow.ReadDocument as XML
import qualified Text.XML.HXT.Arrow.XmlState     as XML


newtype SVDException = SVDException String deriving Show
instance Exception SVDException


-- | Looks up (presumably known) device, throws exception if it can not be done
lookupStmDevice
  :: DeviceModel
  -> IO SVD.Device
lookupStmDevice (DeviceModel m) = do
  (_, bytes) <- maybe (throwM (SVDException ("Can't find " <> m))) pure
    (find ((== m) .fst) stmSvdFileData)

  res <- XML.runX (XML.readString [] (UTF8.toString bytes) >>> SVD.svd)

  case res of
    []  -> (throwM . SVDException) ("No device data for " <> m)
    [x] -> pure x
    _   -> (throwM . SVDException) ("Multiple devices parsed for " <> m)


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
