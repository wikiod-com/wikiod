---
title: "UART - Universal Asynchronous ReceiverTransmitter (serial communication)"
slug: "uart---universal-asynchronous-receivertransmitter-serial-communication"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

This topic is about serial communication using the Universal Asynchronous Receiver/Transmitter (UART) peripheral of the STM32 microcontrollers.

## Echo application - HAL library
In this example the microcontroller echos back the received bytes to the sender using UART RX interrupt.

<!-- language: c -->

    #include "stm32f4xx.h"
    
    UART_HandleTypeDef huart2;
    
    /* Single byte to store input */
    uint8_t byte;
    
    void SystemClock_Config(void);
    
    /* UART2 Interrupt Service Routine */
    void USART2_IRQHandler(void)
    {
      HAL_UART_IRQHandler(&huart2);
    }
    
    /* This callback is called by the HAL_UART_IRQHandler when the given number of bytes are received */
    void HAL_UART_RxCpltCallback(UART_HandleTypeDef *huart)
    {
      if (huart->Instance == USART2)
      {
        /* Transmit one byte with 100 ms timeout */
        HAL_UART_Transmit(&huart2, &byte, 1, 100);
    
        /* Receive one byte in interrupt mode */ 
        HAL_UART_Receive_IT(&huart2, &byte, 1);
      }
    }
    
    void uart_gpio_init()
    {
      GPIO_InitTypeDef GPIO_InitStruct;
    
      __GPIOA_CLK_ENABLE();
    
      /**USART2 GPIO Configuration
      PA2     ------> USART2_TX
      PA3     ------> USART2_RX
      */
      GPIO_InitStruct.Pin = GPIO_PIN_2 | GPIO_PIN_3;
      GPIO_InitStruct.Mode = GPIO_MODE_AF_PP;
      GPIO_InitStruct.Pull = GPIO_PULLUP;
      GPIO_InitStruct.Speed = GPIO_SPEED_LOW;
      GPIO_InitStruct.Alternate = GPIO_AF7_USART2;
      HAL_GPIO_Init(GPIOA, &GPIO_InitStruct);
    }
    
    void uart_init()
    {
      __USART2_CLK_ENABLE();
    
      huart2.Instance = USART2;
      huart2.Init.BaudRate = 115200;
      huart2.Init.WordLength = UART_WORDLENGTH_8B;
      huart2.Init.StopBits = UART_STOPBITS_1;
      huart2.Init.Parity = UART_PARITY_NONE;
      huart2.Init.Mode = UART_MODE_TX_RX;
      huart2.Init.HwFlowCtl = UART_HWCONTROL_NONE;
      huart2.Init.OverSampling = UART_OVERSAMPLING_16;
      HAL_UART_Init(&huart2);
    
      /* Peripheral interrupt init*/
      HAL_NVIC_SetPriority(USART2_IRQn, 0, 0);
      HAL_NVIC_EnableIRQ(USART2_IRQn);
    }
    
    int main(void)
    {
      /* Reset of all peripherals, Initializes the Flash interface and the Systick. */
      HAL_Init();
                 
      uart_gpio_init();
      uart_init();

      HAL_UART_Receive_IT(&huart2, &byte, 1);
    
      while(1)
      {

      }
    }

This example used an STM32F4 Discovery (STM32F407VG), GPIO and alternate function values should be changed according to the STM32 microcontroller in use.

## Transmit large amount of data using DMA and interrupts - HAL library
In this example 2000 bytes will be transfered using DMA, **Transmit Half Complete** and **Transmit Complete** interrupts achieving the best performance.

The first half of the transmit buffer is loaded with new data by the CPU in the **Transmit Half Complete** interrupt callback while the second half of the buffer is being transmitted by the DMA in the background.

Then, in the **Transmit Complete** the second half of the transmit buffer is loaded by the new data by the CPU while the first half (previously updated) is being transmitted by the DMA in the background.

<!-- language: c -->

    #include "stm32f4xx.h"
    
    uint8_t dma_buffer[2000];
    volatile uint8_t toggle = 0;
    
    UART_HandleTypeDef huart2;
    DMA_HandleTypeDef hdma_usart2_tx;
    
    void uart_gpio_init()
    {
      GPIO_InitTypeDef GPIO_InitStruct;
    
      __GPIOA_CLK_ENABLE();
    
      /**USART2 GPIO Configuration
      PA2     ------> USART2_TX
      PA3     ------> USART2_RX
      */
      GPIO_InitStruct.Pin = GPIO_PIN_2|GPIO_PIN_3;
      GPIO_InitStruct.Mode = GPIO_MODE_AF_PP;
      GPIO_InitStruct.Pull = GPIO_PULLUP;
      GPIO_InitStruct.Speed = GPIO_SPEED_HIGH;
      GPIO_InitStruct.Alternate = GPIO_AF7_USART2;
      HAL_GPIO_Init(GPIOA, &GPIO_InitStruct);
    }
    
    void uart_dma_init()
    {
      /* DMA controller clock enable */
      __DMA1_CLK_ENABLE();
    
      /* Peripheral DMA init*/
      hdma_usart2_tx.Instance = DMA1_Stream6;
      hdma_usart2_tx.Init.Channel = DMA_CHANNEL_4;
      hdma_usart2_tx.Init.Direction = DMA_MEMORY_TO_PERIPH;
      hdma_usart2_tx.Init.PeriphInc = DMA_PINC_DISABLE;
      hdma_usart2_tx.Init.MemInc = DMA_MINC_ENABLE;
      hdma_usart2_tx.Init.PeriphDataAlignment = DMA_MDATAALIGN_BYTE;
      hdma_usart2_tx.Init.MemDataAlignment = DMA_MDATAALIGN_BYTE;
      hdma_usart2_tx.Init.Mode = DMA_NORMAL;
      hdma_usart2_tx.Init.Priority = DMA_PRIORITY_LOW;
      hdma_usart2_tx.Init.FIFOMode = DMA_FIFOMODE_DISABLE;
      HAL_DMA_Init(&hdma_usart2_tx);
    
      __HAL_LINKDMA(&huart2,hdmatx,hdma_usart2_tx);
    
      /* DMA interrupt init */
      HAL_NVIC_SetPriority(DMA1_Stream6_IRQn, 0, 0);
      HAL_NVIC_EnableIRQ(DMA1_Stream6_IRQn);
    }
    
    void uart_init()
    {
      __USART2_CLK_ENABLE();
    
      huart2.Instance = USART2;
      huart2.Init.BaudRate = 115200;
      huart2.Init.WordLength = UART_WORDLENGTH_8B;
      huart2.Init.StopBits = UART_STOPBITS_1;
      huart2.Init.Parity = UART_PARITY_NONE;
      huart2.Init.Mode = UART_MODE_TX_RX;
      huart2.Init.HwFlowCtl = UART_HWCONTROL_NONE;
      huart2.Init.OverSampling = UART_OVERSAMPLING_16;
      HAL_UART_Init(&huart2);
    
      /* Peripheral interrupt init*/
      HAL_NVIC_SetPriority(USART2_IRQn, 0, 0);
      HAL_NVIC_EnableIRQ(USART2_IRQn);
    }
    
    /* This function handles DMA1 stream6 global interrupt. */
    void DMA1_Stream6_IRQHandler(void)
    {
      HAL_DMA_IRQHandler(&hdma_usart2_tx);
    }
    
    void USART2_IRQHandler(void)
    {
      HAL_UART_IRQHandler(&huart2);
    }
    
    void HAL_UART_TxCpltCallback(UART_HandleTypeDef *huart)
    {
      uint16_t i;
      toggle = !toggle;
    
      for(i = 1000; i < 1998; i++)
      {
        if(toggle)
          dma_buffer[i] = '&';
        else
          dma_buffer[i] = 'z';
      }
    
      dma_buffer[1998] = '\r';
      dma_buffer[1999] = '\n';
    }
    
    void HAL_UART_TxHalfCpltCallback(UART_HandleTypeDef *huart)
    {
      uint16_t i;
    
      for(i = 0; i < 1000; i++)
      {
        if(toggle)
          dma_buffer[i] = 'y';
        else
          dma_buffer[i] = '|';
      }
    }
    
    int main(void)
    {
      /* Reset of all peripherals, Initializes the Flash interface and the Systick. */
      HAL_Init();

      uart_gpio_init();
      uart_dma_init();
      uart_init();
    
      uint16_t i;
    
      for(i = 0; i < 1998; i++)
      {
        dma_buffer[i] = 'x';
      }
    
      dma_buffer[1998] = '\r';
      dma_buffer[1999] = '\n';
    
      while(1)
      {
        HAL_UART_Transmit_DMA(&huart2, dma_buffer, 2000);
      }
    }

The example was written for an STM32F4 Discovery board (STM32F407VG). The appropriate DMA instance, UART-DMA channel, GPIO and alternate function settings should be changed according to the STM32 microcontroller in use.
    




