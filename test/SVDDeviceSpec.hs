{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SVDDeviceSpec(spec) where


import           GDBDump
import           SVDDevice
import           SVDData
import           Import
import           Test.Hspec
import           Types
import qualified RIO.Map        as Map
import           Data.SVD.Types
import           Text.Printf

spec :: Spec
spec =
  describe "SVDDevice" $ do
    it "should load allRegisterOffsets" $ do
        d <- liftIO $ lookupStmDevice (DeviceModel "STM32F107xx")
        let Just rccPeriph = Map.lookup (Address (rccBoundary + 0x2C)) (allRegisterOffsets d)
        rccPeriph ^. prRegister . to regName `shouldBe` "CFGR2"

    it "should decodeRegisterData" $ do
      d <- liftIO $ lookupStmDevice (DeviceModel "STM32F107xx")
      dumpData <- parseDumpFile "data/STM32F107xx.dump"
      let regData =
            [ ( pfv ^. pfvPeripheral . to periphName
              , pfv ^. pfvRegister . to regName
              , pfv ^. pfvField . to fieldName
              , pfv ^. pfvValue . to (printf ("0b%0" <> show (pfv ^. pfvFieldBitWidth) <> "b"))

              )
            | pfv <- decodeRegisterData (allRegisterOffsets d) dumpData
            ]

      regData `shouldBe`
        [ ("RCC","CR","HSION","0b1")
        , ("RCC","CR","HSIRDY","0b1")
        , ("RCC","CR","HSITRIM","0b10000")
        , ("RCC","CR","HSICAL","0b01100101")
        , ("RCC","CR","HSEON","0b0")
        , ("RCC","CR","HSERDY","0b0")
        , ("RCC","CR","HSEBYP","0b0")
        , ("RCC","CR","CSSON","0b0")
        , ("RCC","CR","PLLON","0b0")
        , ("RCC","CR","PLLRDY","0b0")
        , ("RCC","CR","PLL2ON","0b1")
        , ("RCC","CR","PLL2RDY","0b0")
        , ("RCC","CR","PLL3ON","0b0")
        , ("RCC","CR","PLL3RDY","0b0")
        , ("RCC","CFGR","SW","0b00")
        , ("RCC","CFGR","SWS","0b00")
        , ("RCC","CFGR","HPRE","0b0000")
        , ("RCC","CFGR","PPRE1","0b000")
        , ("RCC","CFGR","PPRE2","0b000")
        , ("RCC","CFGR","ADCPRE","0b00")
        , ("RCC","CFGR","PLLSRC","0b0")
        , ("RCC","CFGR","PLLXTPRE","0b0")
        , ("RCC","CFGR","PLLMUL","0b0000")
        , ("RCC","CFGR","OTGFSPRE","0b0")
        , ("RCC","CFGR","MCO","0b0000")
        , ("RCC","CIR","LSIRDYF","0b0")
        , ("RCC","CIR","LSERDYF","0b0")
        , ("RCC","CIR","HSIRDYF","0b0")
        , ("RCC","CIR","HSERDYF","0b0")
        , ("RCC","CIR","PLLRDYF","0b0")
        , ("RCC","CIR","PLL2RDYF","0b0")
        , ("RCC","CIR","PLL3RDYF","0b0")
        , ("RCC","CIR","CSSF","0b0")
        , ("RCC","CIR","LSIRDYIE","0b0")
        , ("RCC","CIR","LSERDYIE","0b0")
        , ("RCC","CIR","HSIRDYIE","0b0")
        , ("RCC","CIR","HSERDYIE","0b0")
        , ("RCC","CIR","PLLRDYIE","0b0")
        , ("RCC","CIR","PLL2RDYIE","0b0")
        , ("RCC","CIR","PLL3RDYIE","0b0")
        , ("RCC","CIR","LSIRDYC","0b0")
        , ("RCC","CIR","LSERDYC","0b0")
        , ("RCC","CIR","HSIRDYC","0b0")
        , ("RCC","CIR","HSERDYC","0b0")
        , ("RCC","CIR","PLLRDYC","0b0")
        , ("RCC","CIR","PLL2RDYC","0b0")
        , ("RCC","CIR","PLL3RDYC","0b0")
        , ("RCC","CIR","CSSC","0b0")
        , ("RCC","APB2RSTR","AFIORST","0b0")
        , ("RCC","APB2RSTR","IOPARST","0b0")
        , ("RCC","APB2RSTR","IOPBRST","0b0")
        , ("RCC","APB2RSTR","IOPCRST","0b0")
        , ("RCC","APB2RSTR","IOPDRST","0b0")
        , ("RCC","APB2RSTR","IOPERST","0b0")
        , ("RCC","APB2RSTR","ADC1RST","0b0")
        , ("RCC","APB2RSTR","ADC2RST","0b0")
        , ("RCC","APB2RSTR","TIM1RST","0b0")
        , ("RCC","APB2RSTR","SPI1RST","0b0")
        , ("RCC","APB2RSTR","USART1RST","0b0")
        , ("RCC","APB1RSTR","TIM2RST","0b0")
        , ("RCC","APB1RSTR","TIM3RST","0b0")
        , ("RCC","APB1RSTR","TIM4RST","0b0")
        , ("RCC","APB1RSTR","TIM5RST","0b0")
        , ("RCC","APB1RSTR","TIM6RST","0b0")
        , ("RCC","APB1RSTR","TIM7RST","0b0")
        , ("RCC","APB1RSTR","WWDGRST","0b0")
        , ("RCC","APB1RSTR","SPI2RST","0b0")
        , ("RCC","APB1RSTR","SPI3RST","0b0")
        , ("RCC","APB1RSTR","USART2RST","0b0")
        , ("RCC","APB1RSTR","USART3RST","0b0")
        , ("RCC","APB1RSTR","USART4RST","0b0")
        , ("RCC","APB1RSTR","USART5RST","0b0")
        , ("RCC","APB1RSTR","I2C1RST","0b0")
        , ("RCC","APB1RSTR","I2C2RST","0b0")
        , ("RCC","APB1RSTR","CAN1RST","0b0")
        , ("RCC","APB1RSTR","CAN2RST","0b0")
        , ("RCC","APB1RSTR","BKPRST","0b0")
        , ("RCC","APB1RSTR","PWRRST","0b0")
        , ("RCC","APB1RSTR","DACRST","0b0")
        , ("RCC","AHBENR","DMA1EN","0b0")
        , ("RCC","AHBENR","DMA2EN","0b0")
        , ("RCC","AHBENR","SRAMEN","0b1")
        , ("RCC","AHBENR","FLITFEN","0b1")
        , ("RCC","AHBENR","CRCEN","0b0")
        , ("RCC","AHBENR","OTGFSEN","0b0")
        , ("RCC","AHBENR","ETHMACEN","0b0")
        , ("RCC","AHBENR","ETHMACTXEN","0b0")
        , ("RCC","AHBENR","ETHMACRXEN","0b0")
        , ("RCC","APB2ENR","AFIOEN","0b1")
        , ("RCC","APB2ENR","IOPAEN","0b1")
        , ("RCC","APB2ENR","IOPBEN","0b1")
        , ("RCC","APB2ENR","IOPCEN","0b1")
        , ("RCC","APB2ENR","IOPDEN","0b1")
        , ("RCC","APB2ENR","IOPEEN","0b1")
        , ("RCC","APB2ENR","ADC1EN","0b0")
        , ("RCC","APB2ENR","ADC2EN","0b0")
        , ("RCC","APB2ENR","TIM1EN","0b0")
        , ("RCC","APB2ENR","SPI1EN","0b0")
        , ("RCC","APB2ENR","USART1EN","0b0")
        , ("RCC","APB1ENR","TIM2EN","0b0")
        , ("RCC","APB1ENR","TIM3EN","0b0")
        , ("RCC","APB1ENR","TIM4EN","0b0")
        , ("RCC","APB1ENR","TIM5EN","0b0")
        , ("RCC","APB1ENR","TIM6EN","0b0")
        , ("RCC","APB1ENR","TIM7EN","0b0")
        , ("RCC","APB1ENR","WWDGEN","0b0")
        , ("RCC","APB1ENR","SPI2EN","0b0")
        , ("RCC","APB1ENR","SPI3EN","0b0")
        , ("RCC","APB1ENR","USART2EN","0b0")
        , ("RCC","APB1ENR","USART3EN","0b0")
        , ("RCC","APB1ENR","UART4EN","0b0")
        , ("RCC","APB1ENR","UART5EN","0b0")
        , ("RCC","APB1ENR","I2C1EN","0b0")
        , ("RCC","APB1ENR","I2C2EN","0b0")
        , ("RCC","APB1ENR","CAN1EN","0b0")
        , ("RCC","APB1ENR","CAN2EN","0b0")
        , ("RCC","APB1ENR","BKPEN","0b0")
        , ("RCC","APB1ENR","PWREN","0b0")
        , ("RCC","APB1ENR","DACEN","0b0")
        , ("RCC","BDCR","LSEON","0b0")
        , ("RCC","BDCR","LSERDY","0b0")
        , ("RCC","BDCR","LSEBYP","0b0")
        , ("RCC","BDCR","RTCSEL","0b00")
        , ("RCC","BDCR","RTCEN","0b0")
        , ("RCC","BDCR","BDRST","0b0")
        , ("RCC","CSR","LSION","0b0")
        , ("RCC","CSR","LSIRDY","0b0")
        , ("RCC","CSR","RMVF","0b0")
        , ("RCC","CSR","PINRSTF","0b1")
        , ("RCC","CSR","PORRSTF","0b1")
        , ("RCC","CSR","SFTRSTF","0b1")
        , ("RCC","CSR","IWDGRSTF","0b0")
        , ("RCC","CSR","WWDGRSTF","0b0")
        , ("RCC","CSR","LPWRRSTF","0b0")
        , ("RCC","AHBRSTR","OTGFSRST","0b0")
        , ("RCC","AHBRSTR","ETHMACRST","0b0")
        , ("RCC","CFGR2","PREDIV1","0b0100")
        , ("RCC","CFGR2","PREDIV2","0b0100")
        , ("RCC","CFGR2","PLL2MUL","0b0110")
        , ("RCC","CFGR2","PLL3MUL","0b0110")
        , ("RCC","CFGR2","PREDIV1SRC","0b1")
        , ("RCC","CFGR2","I2S2SRC","0b0")
        , ("RCC","CFGR2","I2S3SRC","0b0")
        ]


rccBoundary :: Word32
rccBoundary = 0x40021000
