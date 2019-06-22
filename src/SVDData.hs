{-# LANGUAGE TemplateHaskell #-}

module SVDData
  (
  ) where


import qualified Codec.Archive.Tar as Tar
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.FileEmbed
import           Import




stmTgz :: BS.ByteString
stmTgz = $(embedFile "STMicro.tgz")
