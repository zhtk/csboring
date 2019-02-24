#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "main.h"
#include "usart.h"

char writeBuffer[MAX_WRITE_BUFFER_SIZE];
int writeBufferStart = 0;
int writeBufferStop = 0;

char readBuffer[MAX_READ_BUFFER_SIZE];
int readBufferEnd = 0;

void EnableUSART2() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;
    RCC->APB1ENR |= RCC_APB1ENR_USART2EN;
    
    GPIOafConfigure(USART_TX_GPIO, USART_TX_PIN, GPIO_OType_PP,
        GPIO_Fast_Speed, GPIO_PuPd_NOPULL, GPIO_AF_USART2);
    GPIOafConfigure(USART_RX_GPIO, USART_RX_PIN, GPIO_OType_PP,
        GPIO_Fast_Speed, GPIO_PuPd_UP, GPIO_AF_USART2);
    
    USART2->CR1 = USART_Mode_Rx_Tx | USART_WordLength_8b | USART_Parity_No;
    USART2->CR2 = USART_StopBits_1;
    USART2->CR3 = USART_FlowControl_None;
    USART2->BRR = (PCLK1_HZ + (BAUDRATE / 2U)) / BAUDRATE;
    USART2->CR1 |= USART_Enable;
}

void WriteString(char *str) {
    while (*str) {
        int nextCell = (writeBufferStop + 1) % MAX_WRITE_BUFFER_SIZE;

        if (nextCell == writeBufferStart)
            return;

        writeBuffer[nextCell] = *str;
        writeBufferStop = nextCell;

        ++str;
    }
}

void ProcessWriteBuffer() {
    if (writeBufferStart != writeBufferStop && USART2->SR & USART_SR_TXE) {
        char c = writeBuffer[writeBufferStart++];
        USART2->DR = c;
    }
}

void ProcessReadBuffer() {
    if (USART2->SR & USART_SR_RXNE) {
        char c;
        c = USART2->DR;

        readBuffer[readBufferEnd++] = c;
        if (!ExecuteLEDcommand(readBuffer, readBufferEnd)) {
            memset(readBuffer, 0, MAX_READ_BUFFER_SIZE);
            readBufferEnd = 0;
        }

        if (readBufferEnd >= MAX_READ_BUFFER_SIZE) {
            memset(readBuffer, 0, MAX_READ_BUFFER_SIZE);
            readBufferEnd = 0;
            WriteString("COMMAND TOO LONG, CLEARING BUFFER\r\n");
        }
    }
}
