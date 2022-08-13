---
title: "Getting started with stm32"
slug: "getting-started-with-stm32"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## First time setup with blink LED example using SW4STM32 and HAL library
(**Note:** There are many IDE, toolchain and library which are ready-to-use with STM32. The following setup requires minimal effort to get it work, but it is only one of the many. Feel free to explore others, it is not the purpose of this example to force anyone to use the tools that will be used here.)

----------

## IDE installation ##

[System Workbench for STM32][1]: free IDE on Windows, Linux and OS X. It has been built by [AC6][2] and available for download after registration from the [OpenSTM32 Community's website][3].

The IDE itself is based on Eclipse, but comes with some extras for STM32 development like:

 - Ac6 STM32 MCU GCC toolchain
 - [OpenOCD][4] and GDB (arm-none-eabi-gdb) with automatically generated debug configurations depending on the target board
 - Built-in options to program or erase chip

To start with STM32 before creating your own board, it is recommended to experiment with a [Discovery][5], a [Nucleo][5] or an [Eval board][5], which come with an on-board SWD (Serial Wire Debug) programmer/debugger called ST-Link.

## Creating a project ##

This example will use an [STM32F4 Discovery kit][6], which features an STM32F407VG microcontroller. (Any other board can be used as well.)

 1. Open SW4STM32 and create a new C project: **File &#8594; New &#8594; C Project**
 2. Give it a name like *"STM32F4_Discovery-Blinky"* and from the **Project Type** list choose the **Executable/Ac6 STM32 MCU Project**. By default the only available toolchain is **Ac6 STM32 MCU GCC**. Click Next.

    [![enter image description here][7]][7]

 3. Next step is *Debug/Release settings*, can be skipped now by clicking Next.
 4. *Board selection*. Existing boards can be selected as in this example the STM32F4 Discovery or new custom boards can be added.

    [![enter image description here][8]][8]

 5. Next step is *Project Firmware configuration*. Choose between **No firmware**, **Standard Peripheral Library** (SPL) or **Hardware Abstraction Layer** (HAL). It is questioned which one is more suitable for development, but this question is out of scope in this example. This example will use the HAL library as it is the currently supported by ST Microelectronics. Additional available software tool for HAL is [STM32CubeMX][9], which is an initialization code generator. Also several example applications are available by the [STM32CubeFx][10] or STM32CubeLx software packages. Download the target firmware if it's missing and it is recommended select the **"Add low level drivers in the project"** and the **"As sources in the application"** options. Finally, click Finish.

    [![enter image description here][11]][11]

## Blink LED application ##

As this project has been created with an STM32F4 Discovery, there are already several ready-to-use functions under the **/STM32F4_Discovery-Blinky/Utilities/STM32F4-Discovery/** project folder which can be used to interface the Discovery kit's peripherals (accelerometer, audio, LEDs, push button). In this example the `void BSP_LED_Init(Led_TypeDef Led)` and the `void BSP_LED_Toggle(Led_TypeDef Led)` functions will be used from the *stm32f4_discovery.c* file to blink the green LED, which is `LED4`. To decide which LED is which use the schematics of the [Discovery kit][12].

[![enter image description here][13]][13]

The actual pin and port names are already hidden by some `#define` and `enum`, use *Ctrl + Click* to track them.

 1. Inside the `main`, call the `HAL_Init()` function which resets all peripherals, initializes the Flash interface and the Systick. (Systick will be used to generate delay for the blinking.)
 2. The system clock have to be configured. It can be done by using the [STM32CubeMX clock configuration feature][14] or by the reference manual. In this example the system clock is fed by the internal PLL (Phase Locked Loop), which is sourced by an external 8 MHz crystal oscillator (HSE). Prescalers have been set to achieve the maximum available frequency, which is 168 MHz in case of the F4 Discovery.
 3. Initialization of the peripherals, in this case a GPIO pin.
 4. Inside an endless loop, call the LED toggling and the `HAL_Delay()` function. `HAL_Delay()` uses the `Systick` and generates a delay in miliseconds.

The whole code is the following:

<!-- language: c -->

    #include "stm32f4xx.h"
    #include "stm32f4_discovery.h"
                
    void SystemClock_Config(void);
    
    int main(void)
    {
        /* Reset of all peripherals, Initializes the Flash interface and the Systick. */
        HAL_Init();
    
        /* Configure the system clock */
        SystemClock_Config();
    
        /* Initialize one of the LED GPIO pin */
        BSP_LED_Init(LED4);
    
        while(1)
        {
            BSP_LED_Toggle(LED4);
            HAL_Delay(1000);       // in miliseconds
        }
    }
    
    /**
     * @brief  System Clock Configuration
     *         The system Clock is configured as follow :
     *            System Clock source            = PLL (HSE)
     *            SYSCLK(Hz)                     = 168000000
     *            HCLK(Hz)                       = 168000000
     *            AHB Prescaler                  = 1
     *            APB1 Prescaler                 = 4
     *            APB2 Prescaler                 = 2
     *            HSE Frequency(Hz)              = HSE_VALUE
     *            PLL_M                          = (HSE_VALUE/1000000u)
     *            PLL_N                          = 336
     *            PLL_P                          = 2
     *            PLL_Q                          = 7
     *            VDD(V)                         = 3.3
     *            Main regulator output voltage  = Scale1 mode
     *            Flash Latency(WS)              = 5
     * @param  None
     * @retval None
     */
    void SystemClock_Config(void)
    {
      RCC_ClkInitTypeDef RCC_ClkInitStruct;
      RCC_OscInitTypeDef RCC_OscInitStruct;
    
      // Enable Power Control clock
      __PWR_CLK_ENABLE();
    
      // The voltage scaling allows optimizing the power consumption when the
      // device is clocked below the maximum system frequency, to update the
      // voltage scaling value regarding system frequency refer to product
      // datasheet.
      __HAL_PWR_VOLTAGESCALING_CONFIG(PWR_REGULATOR_VOLTAGE_SCALE1);
    
      // Enable HSE Oscillator and activate PLL with HSE as source
      RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_HSE;
      RCC_OscInitStruct.HSEState = RCC_HSE_ON;
      RCC_OscInitStruct.PLL.PLLState = RCC_PLL_ON;
      RCC_OscInitStruct.PLL.PLLSource = RCC_PLLSOURCE_HSE;
    
      // This assumes the HSE_VALUE is a multiple of 1MHz. If this is not
      // your case, you have to recompute these PLL constants.
      RCC_OscInitStruct.PLL.PLLM = (HSE_VALUE/1000000u);
      RCC_OscInitStruct.PLL.PLLN = 336;
      RCC_OscInitStruct.PLL.PLLP = RCC_PLLP_DIV2;
      RCC_OscInitStruct.PLL.PLLQ = 7;
      HAL_RCC_OscConfig(&RCC_OscInitStruct);
    
      // Select PLL as system clock source and configure the HCLK, PCLK1 and PCLK2
      // clocks dividers
      RCC_ClkInitStruct.ClockType = (RCC_CLOCKTYPE_SYSCLK | RCC_CLOCKTYPE_HCLK
          | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2);
      RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
      RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
      RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV4;
      RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV2;
      HAL_RCC_ClockConfig(&RCC_ClkInitStruct, FLASH_LATENCY_5);
    }

Build with the hammer [![enter image description here][15]][15], and download the application by right clicking on the project folder and selecting the **Target &#8594; Program chip...** option.

Another way to download is with using *debug*. To do so click on the arrow beside the bug icon [![enter image description here][16]][16] in the toolbar and open **Debug Configuration...** menu. Creat a new **Ac6 STM32 Debugging** configuration and if the **C/C++ Application** field is empty, fill in the following:

> Debug\STM32F4_Discovery-Blinky.elf

Other debug parameters such as the OpenOCD configuration file and the used Telnet and GDB ports are automatically generated and filled in by the framework. Finally, click the Debug button.


  [1]: http://www.st.com/en/development-tools/sw4stm32.html
  [2]: http://www.ac6.fr/index.php/lang_en%5FGB.xphp
  [3]: http://www.openstm32.org/HomePage
  [4]: http://openocd.org/
  [5]: http://www.st.com/content/st_com/en/products/evaluation-tools/product-evaluation-tools/mcu-eval-tools/stm32-mcu-eval-tools.html?querycriteria=productId=SS1532
  [6]: http://www.st.com/en/evaluation-tools/stm32f4discovery.html
  [7]: https://i.stack.imgur.com/fFzhZ.png
  [8]: https://i.stack.imgur.com/ZwC2d.png
  [9]: http://www.st.com/en/development-tools/stm32cubemx.html
  [10]: http://www.st.com/en/embedded-software/stm32cubef4.html
  [11]: https://i.stack.imgur.com/LLTl7.png
  [12]: http://www.st.com/content/ccc/resource/technical/document/user_manual/70/fe/4a/3f/e7/e1/4f/7d/DM00039084.pdf/files/DM00039084.pdf/jcr:content/translations/en.DM00039084.pdf
  [13]: https://i.stack.imgur.com/Uq7JC.png
  [14]: https://elektronotes.files.wordpress.com/2015/02/stm32cubemx_clockconf.png
  [15]: https://i.stack.imgur.com/eAZOV.png
  [16]: https://i.stack.imgur.com/HKHOB.png

