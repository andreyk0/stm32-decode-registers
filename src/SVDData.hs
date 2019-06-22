{-# LANGUAGE TemplateHaskell #-}

module SVDData
  ( printTarEntries
  ) where


import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LB
import           Data.FileEmbed
import           Import



printTarEntries :: IO ()
printTarEntries = printNextEntry stmTarEntries
  where
    printNextEntry Tar.Done        = pure ()
    printNextEntry (Tar.Fail e)    = print e
    printNextEntry (Tar.Next e es) = print (Tar.entryPath e) >> printNextEntry es



stmTarEntries :: Tar.Entries Tar.FormatError
stmTarEntries  = Tar.read $ GZ.decompress stmTgz


stmTgz :: LB.ByteString
stmTgz = LB.fromStrict $(embedFile "STMicro.tgz")
