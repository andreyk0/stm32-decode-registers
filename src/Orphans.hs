{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Orphans where


import           RIO
import qualified RIO.Text as Text


instance Display [Char] where
  textDisplay = Text.pack
