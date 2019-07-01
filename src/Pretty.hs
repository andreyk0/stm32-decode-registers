{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}


module Pretty
  ( ppPeripheralFieldValues
  , ppPeripheralRegisters
  , ppWord32Hex
  ) where


import           Data.Monoid
import           Data.Semigroup
import           Data.Text.Prettyprint.Doc
import           Import
import           Text.Printf
import           Types


type MaxWidthPr = (Max Int, Max Int)
type MaxWidthPfv = (Max Int, Max Int, Max Int)


ppPeripheralFieldValues
  :: [PeripheralFieldValue]
  -> Doc ann
ppPeripheralFieldValues pfvs =
  vcat (ppPeripheralFieldValue mw <$> pfvs) <+>
  line
  where
    mw = foldMap (\x ->
                    ( (Max . length . view pfvPeripheralName) x
                    , (Max . length . view pfvRegisterName) x
                    , (Max . length . view pfvFieldName) x
                    )
                 ) pfvs


ppPeripheralFieldValue
  :: MaxWidthPfv
  -> PeripheralFieldValue
  -> Doc ann
ppPeripheralFieldValue (coerce -> mwP, coerce -> mwR, coerce -> mwF) pfv =
  (pfv ^. pfvPeripheralName . to (fill mwP . pretty)) <+>
  (pfv ^. pfvRegisterName . to (fill mwR . pretty))  <+>
  (pfv ^. pfvFieldName . to (fill mwF . pretty)) <+>
  (pfv ^. pfvValue . to ppWord32Hex) <+>
  (pfv ^. pfvValue . to (ppWord32Bin (pfv ^. pfvFieldBitWidth)))



ppPeripheralRegisters
  :: [PeripheralRegister]
  -> Doc ann
ppPeripheralRegisters prs =
  vcat (ppPeripheralRegister mw <$> prs) <+>
  line
  where
    mw = foldMap ( Max . length . view prPeripheralName
               &&& Max . length . view prRegisterName
                 ) prs


ppPeripheralRegister
  :: MaxWidthPr
  -> PeripheralRegister
  -> Doc ann
ppPeripheralRegister (coerce -> mwP, coerce -> mwR) pr =
  (pr ^. prAddress . to ppWord32Hex) <+>
  (pr ^. prPeripheralName . to (fill mwP . pretty)) <+>
  (pr ^. prRegisterName . to (fill mwR . pretty))  <+>
  if pr ^. prRegisterName == pr ^. prRegisterDescription
    then mempty
    else pr ^. prRegisterDescription . to pretty


ppWord32Hex
  :: forall a ann . (Coercible a Word32)
  => a
  -> Doc ann
ppWord32Hex w = pretty @String $
  printf "0x%08x" (coerce w :: Word32)


ppWord32Bin
  :: forall a ann . (Coercible a Word32)
  => Int
  -> a
  -> Doc ann
ppWord32Bin numZ w = pretty @String $
  printf ("0b%0" <> show numZ <> "b") (coerce w :: Word32)
