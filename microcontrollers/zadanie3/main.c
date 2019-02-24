#include <delay.h>
#include <gpio.h>
#include <stm32.h>
#include <string.h>

#define LIS35DE_ADDR 0x1C
#define I2C_SPEED_HZ 100000
#define PCLK1_MHZ 16

#define setRedPower(val) TIM3->CCR1 = val
#define setGreenPower(val) TIM3->CCR2 = val
#define setBluePower(val) TIM3->CCR3 = val

void EnableI2C() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOBEN;
    RCC->APB1ENR |= RCC_APB1ENR_I2C1EN;
    
    GPIOafConfigure(GPIOB, 8, GPIO_OType_OD, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_I2C1);
    GPIOafConfigure(GPIOB, 9, GPIO_OType_OD, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_I2C1);

    I2C1->CR1 = 0;
        
    I2C1->CR2 = PCLK1_MHZ;
    I2C1->CCR = (PCLK1_MHZ * 1000000) /    (I2C_SPEED_HZ << 1);
    I2C1->TRISE = PCLK1_MHZ + 1;
    
    I2C1->CR1 |= I2C_CR1_PE;
}

void EnablePWM() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;
    RCC->APB1ENR |= RCC_APB1ENR_TIM3EN;
    
    GPIOafConfigure(GPIOA, 6, GPIO_OType_PP, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_TIM3);
    GPIOafConfigure(GPIOA, 7, GPIO_OType_PP, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_TIM3);
    GPIOafConfigure(GPIOB, 0, GPIO_OType_PP, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_TIM3);

    TIM3->PSC = 0;
    TIM3->ARR = 255;
    TIM3->EGR = TIM_EGR_UG;
    
    TIM3->CCR1 = 255;
    TIM3->CCR2 = 255;
    TIM3->CCR3 = 255;
    
    TIM3->CCMR1 = TIM_CCMR1_OC1M_2 | TIM_CCMR1_OC1M_1 | TIM_CCMR1_OC1PE |
                  TIM_CCMR1_OC2M_2 | TIM_CCMR1_OC2M_1 | TIM_CCMR1_OC2PE;
    TIM3->CCMR2 = TIM_CCMR2_OC3M_2 | TIM_CCMR2_OC3M_1 | TIM_CCMR2_OC3PE;
    TIM3->CCER = TIM_CCER_CC1E | TIM_CCER_CC1P |
                 TIM_CCER_CC2E | TIM_CCER_CC2P |
                 TIM_CCER_CC3E | TIM_CCER_CC3P;
    
    TIM3->CR1 = TIM_CR1_ARPE | TIM_CR1_CEN;
}

#define waitFor(cond) {\
    int clock = 0; \
    while (!(cond)) \
        if (++clock >= 100000) { \
            I2C1->CR1 |= I2C_CR1_STOP; \
            return 0; \
        } \
    }

uint8_t ReadI2C(uint8_t reg) {    
    I2C1->CR1 |= I2C_CR1_START;    
    waitFor(I2C1->SR1 & I2C_SR1_SB);
    
    I2C1->DR = LIS35DE_ADDR << 1;
    waitFor(I2C1->SR1 & I2C_SR1_ADDR);
    
    I2C1->SR2;
    I2C1->DR = reg;
    waitFor(I2C1->SR1 & I2C_SR1_BTF);
    
    I2C1->CR1 |= I2C_CR1_START;
    waitFor(I2C1->SR1 & I2C_SR1_SB);
    I2C1->DR = (LIS35DE_ADDR << 1) | 1U;
    I2C1->CR1 &= ~I2C_CR1_ACK;
    waitFor(I2C1->SR1 & I2C_SR1_ADDR);
    I2C1->SR2;
    
    I2C1->CR1 |= I2C_CR1_STOP;
    waitFor(I2C1->SR1 & I2C_SR1_RXNE);
    
    return I2C1->DR;
}

int main() {
    __disable_irq();
    EnableI2C();
    EnablePWM();
    __enable_irq();

    for (;;) {
        uint8_t regX = ReadI2C(0x29);
        uint8_t regY = ReadI2C(0x2B);
        uint8_t regZ = ReadI2C(0x2D);
        
        setRedPower(regX);
        setGreenPower(regY);
        setBluePower(regZ);
    }
}
