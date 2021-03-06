{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where


import           Import
import           Options.Applicative.Simple
import qualified Paths_stm32_decode_registers
import           RIO.Process
import           Run
import           Types


main :: IO ()
main = do
  let deviceModelOpt = DeviceModel <$> strOption (short 'm' <> long "model" <> help "Device model")

  (options, cmd) <- simpleOptions
    $(simpleVersion Paths_stm32_decode_registers.version)
    "Decode some STM32 registers from a GDB memory dump"
    "E.g. STM32F1 'x/64xb 0x40021000'"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    (do addCommand "list"
                    "List devices"
                    (const CMDList)
                    (pure ())

        addCommand "print-svd"
                    "Print device SVD database info"
                    CMDPrintSVD deviceModelOpt

        addCommand "print-registers"
                    "Print absolute register addresses"
                    CMDPrintRegisters deviceModelOpt

        addCommand "decode"
                    "Decode GDB memory dump"
                    id
                    ( CMDDecode
                   <$> deviceModelOpt
                   <*> strOption (short 'f' <> long "file" <> help "HEX memory dump file from GDB")
                    )
    )

  lo <- logOptionsHandle stderr (options ^. optVerbose)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = options
          }
     in runRIO app (run cmd)
