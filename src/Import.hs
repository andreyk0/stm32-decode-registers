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
  :: ( Display a
     , MonadIO m
     )
  => a
  -> m ()
say' = say . textDisplay


sayErr'
  :: ( Display a
     , MonadIO m
     )
  => a
  -> m ()
sayErr' = sayErr . textDisplay
