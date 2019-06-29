{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Pretty
  ( ppPeripheralRegister
  , ppPeripheralRegisters
  , ppWord32Hex
  ) where


import           Data.SVD.Types
import           Data.Text.Prettyprint.Doc
import           Import
import           Text.Printf
import           Types


ppPeripheralRegisters
  :: [PeripheralRegister]
  -> Doc ann
ppPeripheralRegisters prs =
  vcat $ ppPeripheralRegister <$> prs


ppPeripheralRegister
  :: PeripheralRegister
  -> Doc ann
ppPeripheralRegister pr =
  ppWord32Hex (pr ^. prAddress) <+>
  fill 8 (pretty (periphName (pr ^. prPeripheral))) <+>
  fill 8 (pretty (regName (pr ^. prRegister))) <+>
  pretty (regDescription (pr ^. prRegister))


ppWord32Hex
  :: forall a ann . (Coercible a Word32)
  => a
  -> Doc ann
ppWord32Hex w = pretty @String $
  printf "0x%08x" (coerce w :: Word32)
