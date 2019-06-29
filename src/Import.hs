{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Import
  ( module Data.Coerce
  , module Lens.Micro
  , module Orphans
  , module RIO
  , module Say
  , say'
  , sayErr'
  ) where


import           Data.Coerce
import           Lens.Micro
import           Orphans     ()
import           RIO
import           Say


say'
  :: ( MonadIO m
     , Display a
     )
  => a
  -> m ()
say' = say . textDisplay


sayErr'
  :: ( MonadIO m
     , Display a
     )
  => a
  -> m ()
sayErr' = sayErr . textDisplay
