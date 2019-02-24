#include <delay.h>
#include <gpio.h>
#include <stm32.h>
#include <stdio.h>
#include <string.h>
#include <lcd.h>

#include "main.h"
#include "i2c.h"
#include "sensors.h"
#include "usart.h"

void EnableSensorsCounter(void) {
    __disable_irq();
    
    // Włączenie taktowania układu licznika
    RCC->APB1ENR |= RCC_APB1ENR_TIM3EN;
    
    // Zliczanie w górę
    TIMER_DEV->CR1 = 0;
    
    // Licznik wyzwalany co sekundę
    // Domyślne taktowanie licznika 16 MHz
    TIMER_DEV->PSC = TIMER_PSC;
    TIMER_DEV->ARR = TIMER_ARR;
    
    // Reset licznika
    TIMER_DEV->EGR = TIM_EGR_UG;
    
    // Włączenie przerwań w liczniku
    TIMER_DEV->SR = ~TIM_SR_UIF;
    TIMER_DEV->DIER = TIM_DIER_UIE;
    
    // Konfiguracja NVIC
    NVIC_EnableIRQ(TIMER_NVIC_IRQ);
    
    // Uruchomienie licznika
    TIMER_DEV->CR1 |= TIM_CR1_CEN;
    
    __enable_irq();
}

void TIM3_IRQHandler(void) {
    uint32_t status = TIMER_DEV->SR & TIMER_DEV->DIER;
    
    if (status & TIM_SR_UIF) {
        TIMER_DEV->SR = ~TIM_SR_UIF;
        
        ReadAllSensors();
    }
}

void PrintOnLCD(char *str) {
    LCDgoto(0, 0);

    while (*str)
        LCDputchar(*str++);
}

void PrintSensorsData(void) {
    char message[LCD_MESSAGE_BUFFER];

    sprintf(message, "TEM %5.1f\nPRS %5d\nHUM %5.1f\nLUX %5.1f\n",
        GetTemperature(), GetPressure(), GetHumidity(), GetLight());
 
    PrintOnLCD(message);
 }

void TemperatureReadCallback(int status) {
    PrintSensorsData();
}

void EnableLightSensorCallback(int status) {
    EnableSensorsCounter();
}

int main() {
    __disable_irq();
    
    EnableUSART2();
    EnableI2C();
    
    LCDconfigure();
    LCDclear();
    
    EnablePressureSensor();
    EnableHumiditySensor();
    EnableLightSensor();

    __enable_irq();

    for (;;) ;
}
