---
title : stm32 Tutorial
slug : stm32-tutorial
weight : 9740
draft : false
images : []
type : docs
---

This section provides an overview of what stm32 is, and why a developer might want to use it.

It should also mention any large subjects within stm32, and link out to the related topics.  Since the Documentation for stm32 is new, you may need to create initial versions of those related topics.

## What is STM32?
---
STM32 is a 32-bit Flash microcontroller family developed by ST Microelectronics. It is based on the ARM® Cortex®‑M processor and offers a 32‑bit product range that combines very high performance, real-time capabilities, digital signal processing, and low‑power, low‑voltage operation.

A detailed description about each series, development tools and part number decoding can be found on [Wikipedia][15].


Product series
--------------

|       | Cortex-M0 / -M0+ | Cortex-M3| Cortex-M4| Cortex-M7|
|-------| ------ | ------ | ------ | ------ |
| **High performance:**|  | [STM32F2][1] | [STM32F4][2] | [STM32F7][3], [STM32H7][4] |
| **Mainstream:**| [STM32F0][5] | [STM32F1][6] | [STM32F3][7] |  |
| **Ultra-low-power:**| [STM32L0][8]| [STM32L1][9] | [STM32L4][10] |  |

Development boards
------------------

|       | [STM32 Nucleo][11] ([mbed enabled][12])| [Discovery kits][13] | [Evaluation boards][14] |
|----------------| ------ | ------ | ------|
|**Typical use case:**| Flexible prototyping, community   | Prototyping, creative demos| Full feature evaluation |
|**Extension possibilities:**| +++ | ++ | ++|
|**Connectivity:**| Arduino&trade;, ST, Morpho | ST | ST|


  [1]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f2-series.html?querycriteria=productId=SS1575
  [2]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f4-series.html?querycriteria=productId=SS1577
  [3]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f7-series.html?querycriteria=productId=SS1858
  [4]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32h7-series.html?querycriteria=productId=SS1951
  [5]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f0-series.html?querycriteria=productId=SS1574
  [6]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f1-series.html?querycriteria=productId=SS1031
  [7]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32f3-series.html?querycriteria=productId=SS1576
  [8]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32l0-series.html?querycriteria=productId=SS1817
  [9]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32l1-series.html?querycriteria=productId=SS1295
  [10]: http://www.st.com/content/st_com/en/products/microcontrollers/stm32-32-bit-arm-cortex-mcus/stm32l4-series.html?querycriteria=productId=SS1580
  [11]: http://www.st.com/content/st_com/en/products/evaluation-tools/product-evaluation-tools/mcu-eval-tools/stm32-mcu-eval-tools/stm32-mcu-nucleo.html?querycriteria=productId=LN1847
  [12]: https://developer.mbed.org/teams/ST/
  [13]: http://www.st.com/content/st_com/en/products/evaluation-tools/product-evaluation-tools/mcu-eval-tools/stm32-mcu-eval-tools/stm32-mcu-discovery-kits.html?querycriteria=productId=LN1848
  [14]: http://www.st.com/content/st_com/en/products/evaluation-tools/product-evaluation-tools/mcu-eval-tools/stm32-mcu-eval-tools/stm32-mcu-eval-boards.html?querycriteria=productId=LN1199
[15]: https://en.wikipedia.org/wiki/STM32

