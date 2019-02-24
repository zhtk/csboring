#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "usart.h"

char writeBuffer[MAX_WRITE_BUFFER_SIZE];
int writeBufferStart = 0;
int writeBufferStop = 0;
uint32_t dmaWrittenData = 0;

void EnableUSART2(void) {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_DMA1EN;
    RCC->APB1ENR |= RCC_APB1ENR_USART2EN;
    
    GPIOafConfigure(USART_TX_GPIO, USART_TX_PIN, GPIO_OType_PP,
        GPIO_Fast_Speed, GPIO_PuPd_NOPULL, GPIO_AF_USART2);

    // Konfiguracja parametrów transmisji
    USART2->CR1 = USART_CR1_TE;
	USART2->CR2 = 0;
	USART2->BRR = (PCLK1_HZ + (BAUDRATE / 2U)) / BAUDRATE;
	USART2->CR3 = USART_CR3_DMAT;
	
	// Konfiguracja DMA
	DMA1_Stream6->CR = 4U << 25 | DMA_SxCR_PL_1 | DMA_SxCR_MINC
		| DMA_SxCR_DIR_0 | DMA_SxCR_TCIE;
	DMA1_Stream6->PAR = (uint32_t) &USART2->DR;
	
	// Aktywacja przerwań DMA
	DMA1->HIFCR = DMA_HIFCR_CTCIF6 | DMA_HIFCR_CTCIF5;
	NVIC_SetPriority(DMA1_Stream6_IRQn, USART_IRQ_PRIORITY);
	NVIC_EnableIRQ(DMA1_Stream6_IRQn);
	
	// Aktywacja urządzenia
	USART2->CR1 |= USART_CR1_UE;
}

// Przerwanie strumienia wysyłającego
void DMA1_Stream6_IRQHandler(void) {
	uint32_t isr = DMA1->HISR;
	
	if (isr & DMA_HISR_TCIF6) {
		DMA1->HIFCR = DMA_HIFCR_CTCIF6;
		
		writeBufferStart += dmaWrittenData;
		writeBufferStart %= MAX_WRITE_BUFFER_SIZE;
		
		ProcessWriteBuffer();
	}
}

uint8_t IsDMAwriteReady(void) {
	uint32_t dmaEmpty = (DMA1_Stream6->CR & DMA_SxCR_EN) == 0 &&
		(DMA1->HISR & DMA_HISR_TCIF6) == 0;
	
	return dmaEmpty && writeBufferStart != writeBufferStop;
}

void WriteString(char *str) {
	__disable_irq();
	
    while (*str) {
        int nextCell = (writeBufferStop + 1) % MAX_WRITE_BUFFER_SIZE;

        if (nextCell == writeBufferStart)
			break;

        writeBuffer[nextCell] = *str;
        writeBufferStop = nextCell;

        ++str;
    }
    
    ProcessWriteBuffer();
    
    __enable_irq();
}

void ProcessWriteBuffer(void) {
	if (!IsDMAwriteReady())
		return;
	
	if (writeBufferStart < writeBufferStop)
		dmaWrittenData = writeBufferStop - writeBufferStart;
	else
		dmaWrittenData = MAX_WRITE_BUFFER_SIZE - writeBufferStart;
	
	DMA1_Stream6->M0AR = (uint32_t) (writeBuffer + writeBufferStart);
	DMA1_Stream6->NDTR = dmaWrittenData;
	DMA1_Stream6->CR |= DMA_SxCR_EN;
}
