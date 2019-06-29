{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module Pretty
  ( ppPeripheralRegister
  , ppPeripheralRegisters
  , ppWord32Hex
  ) where


import           Data.Text.Prettyprint.Doc
import           Import
import           Text.Printf
import           Types


ppPeripheralRegisters
  :: [PeripheralRegister]
  -> Doc ann
ppPeripheralRegisters prs =
  vcat $ ppPeripheralRegister' mw <$> prs
  where
    mw = foldMap maxWidth prs


ppPeripheralRegister
  :: PeripheralRegister
  -> Doc ann
ppPeripheralRegister = ppPeripheralRegister' maxWidthDef


ppPeripheralRegister'
  :: MaxWidth
  -> PeripheralRegister
  -> Doc ann
ppPeripheralRegister' MaxWidth{..} pr =
  (pr ^. prAddress . to ppWord32Hex) <+>
  (pr ^. prPeripheralName . to (fill mwPeripheralName . pretty)) <+>
  (pr ^. prRegisterName . to (fill mwRegisterName . pretty))  <+>
  if pr ^. prRegisterName == pr ^. prRegisterDescription
    then mempty
    else pr ^. prRegisterDescription . to pretty


ppWord32Hex
  :: forall a ann . (Coercible a Word32)
  => a
  -> Doc ann
ppWord32Hex w = pretty @String $
  printf "0x%08x" (coerce w :: Word32)



data MaxWidth = MaxWidth
  { mwPeripheralName :: !Int
  , mwRegisterName   :: !Int
  } deriving (Eq, Ord, Show)

instance Semigroup MaxWidth where
  (MaxWidth !p0 !r0) <> (MaxWidth !p1 !r1) = MaxWidth (max p0 p1) (max r0 r1)

instance Monoid MaxWidth where
  mempty = MaxWidth 0 0


maxWidthDef :: MaxWidth
maxWidthDef = MaxWidth 20 30

maxWidth
  :: PeripheralRegister
  -> MaxWidth
maxWidth pr =
  MaxWidth (pr ^. prPeripheralName . to length)
           (pr ^. prRegisterName . to length)
