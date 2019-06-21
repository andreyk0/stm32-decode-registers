{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_stm32_decode_registers
import qualified RIO.List                     as L
import qualified RIO.Map                      as Map
import           RIO.Process
import           Run


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_stm32_decode_registers.version)
    "Decode some STM32 registers from a GDB memory dump"
    "E.g. STM32F1 'x/64xb 0x40021000'"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option (maybeReader (`Map.lookup` memMaps))
                  ( long "memory-map"
                 <> short 'm'
                 <> help ("Memory map: " <> L.intercalate " " (Map.keys memMaps))
                  )
    )
    empty

  lo <- logOptionsHandle stderr (optVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
