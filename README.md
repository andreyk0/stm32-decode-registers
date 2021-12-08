# stm32-decode-registers

Little utility to decode GDB memory dump into a human readable form (looking for known registers).

SVD [description](https://www.keil.com/pack/doc/CMSIS/SVD/html/svd_Format_pg.html),
[data](https://github.com/posborne/cmsis-svd)

# Build

Needs [haskell stack](https://docs.haskellstack.org/en/stable/README/) and (optionally) [upx](https://upx.github.io/)

```bash
make build-dist
```

# Running

Find device

``` bash
$ stm32-decode-registers list | rg -i stm32f107
STM32F107xx
```

Lookup register addresses

``` bash
$ stm32-decode-registers print-registers --model STM32F107xx | rg RCC | rg CFGR
0x40021004 RCC            CFGR              Clock configuration register (RCC_CFGR)
0x4002102c RCC            CFGR2             Clock configuration register2 (RCC_CFGR2)
```

Grab a memory dump (gdb)

``` gdb
x/64xb 0x40021000
```

, save output to file, e.g. `dump`.

Decode

``` bash
$ stm32-decode-registers decode --model STM32F107xx --file data/STM32F107xx.dump | head -5
RCC CR       HSION      0x00000001 0b1
RCC CR       HSIRDY     0x00000001 0b1
RCC CR       HSITRIM    0x00000010 0b10000
RCC CR       HSICAL     0x00000065 0b01100101
RCC CR       HSEON      0x00000000 0b0
..........
```
