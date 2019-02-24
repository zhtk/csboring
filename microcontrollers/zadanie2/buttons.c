#include <gpio.h>
#include <stm32.h>

#include "main.h"
#include "buttons.h"
#include "usart.h"

#define READ_BUTTON_USER      (!(BUTTON_USER_GPIO->IDR & 1U << BUTTON_USER_PIN))
#define READ_BUTTON_JOY_LEFT  (!(BUTTON_JOY_LEFT_GPIO->IDR & 1U << BUTTON_JOY_LEFT_PIN))
#define READ_BUTTON_JOY_RIGHT (!(BUTTON_JOY_RIGHT_GPIO->IDR & 1U << BUTTON_JOY_RIGHT_PIN))
#define READ_BUTTON_JOY_UP    (!(BUTTON_JOY_UP_GPIO->IDR & 1U << BUTTON_JOY_UP_PIN))
#define READ_BUTTON_JOY_DOWN  (!(BUTTON_JOY_DOWN_GPIO->IDR & 1U << BUTTON_JOY_DOWN_PIN))
#define READ_BUTTON_JOY_FIRE  (!(BUTTON_JOY_FIRE_GPIO->IDR & 1U << BUTTON_JOY_FIRE_PIN))
#define READ_BUTTON_MODE      (BUTTON_MODE_GPIO->IDR & 1U << BUTTON_MODE_PIN)

#define ButtonConfigure(BUTTON_GPIO, BUTTON_PIN) GPIOinConfigure(BUTTON_GPIO, \
    BUTTON_PIN, GPIO_OType_PP, EXTI_Mode_Interrupt, EXTI_Trigger_Rising_Falling);

char *buttonNames[BUTTON_COUNT] = {
    "USER",
    "LEFT",
    "RIGHT",
    "UP",
    "DOWN",
    "FIRE",
    "MODE"
};

uint32_t buttonInterrupts[BUTTON_COUNT] = {
    BUTTON_USER_EXTI,
    BUTTON_JOY_LEFT_EXTI,
    BUTTON_JOY_RIGHT_EXTI,
    BUTTON_JOY_UP_EXTI,
    BUTTON_JOY_DOWN_EXTI,
    BUTTON_JOY_FIRE_EXTI,
    BUTTON_MODE_EXTI
};

void EnableButtons(void) {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_GPIOBEN | RCC_AHB1ENR_GPIOCEN;
    RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;
    
    ButtonConfigure(BUTTON_USER_GPIO, BUTTON_USER_PIN);
    ButtonConfigure(BUTTON_JOY_LEFT_GPIO, BUTTON_JOY_LEFT_PIN);
    ButtonConfigure(BUTTON_JOY_RIGHT_GPIO, BUTTON_JOY_RIGHT_PIN);
    ButtonConfigure(BUTTON_JOY_UP_GPIO, BUTTON_JOY_UP_PIN);
    ButtonConfigure(BUTTON_JOY_DOWN_GPIO, BUTTON_JOY_DOWN_PIN);
    ButtonConfigure(BUTTON_JOY_FIRE_GPIO, BUTTON_JOY_FIRE_PIN);
    ButtonConfigure(BUTTON_MODE_GPIO, BUTTON_MODE_PIN);
    
    NVIC_SetPriority(EXTI0_IRQn, CUSTOM_IRQ_PRIORITY);
    NVIC_SetPriority(EXTI3_IRQn, CUSTOM_IRQ_PRIORITY);
    NVIC_SetPriority(EXTI4_IRQn, CUSTOM_IRQ_PRIORITY);
    NVIC_SetPriority(EXTI9_5_IRQn, CUSTOM_IRQ_PRIORITY);
    NVIC_SetPriority(EXTI15_10_IRQn, CUSTOM_IRQ_PRIORITY);
    
    NVIC_EnableIRQ(EXTI0_IRQn);
    NVIC_EnableIRQ(EXTI3_IRQn);
    NVIC_EnableIRQ(EXTI4_IRQn);
    NVIC_EnableIRQ(EXTI9_5_IRQn);
    NVIC_EnableIRQ(EXTI15_10_IRQn);
}

void EXTI0_IRQHandler(void) {
    ReadButtons();
}

void EXTI3_IRQHandler(void) {
    ReadButtons();
}

void EXTI4_IRQHandler(void) {
    ReadButtons();
}

void EXTI9_5_IRQHandler(void) {
    ReadButtons();
}

void EXTI15_10_IRQHandler(void) {
    ReadButtons();
}

void ReadButtons(void) {
    char buttonStates[BUTTON_COUNT] = {
        READ_BUTTON_USER,
        READ_BUTTON_JOY_LEFT,
        READ_BUTTON_JOY_RIGHT,
        READ_BUTTON_JOY_UP,
        READ_BUTTON_JOY_DOWN,
        READ_BUTTON_JOY_FIRE,
        READ_BUTTON_MODE
    };
    
    uint32_t interrupts = EXTI->PR;
    
    for (int i = 0; i < BUTTON_COUNT; ++i) {
        if (interrupts & buttonInterrupts[i]) {                
            WriteString(buttonNames[i]);

            if (buttonStates[i])
                WriteString(" PRESSED\r\n");
            else
                WriteString(" RELEASED\r\n");
        }

        EXTI->PR = buttonInterrupts[i];
    }
}
