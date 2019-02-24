#include <gpio.h>
#include <stm32.h>

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
    BUTTON_PIN, GPIO_OType_PP, GPIO_Low_Speed, GPIO_PuPd_NOPULL);

uint8_t buttonStates[BUTTON_COUNT];
uint16_t buttonClock[BUTTON_COUNT];
char *buttonNames[BUTTON_COUNT] = {
    "USER",
    "LEFT",
    "RIGHT",
    "UP",
    "DOWN",
    "FIRE",
    "MODE"
};

void EnableButtons() {
    ButtonConfigure(BUTTON_USER_GPIO, BUTTON_USER_PIN);

    ButtonConfigure(BUTTON_JOY_LEFT_GPIO, BUTTON_JOY_LEFT_PIN);
    ButtonConfigure(BUTTON_JOY_RIGHT_GPIO, BUTTON_JOY_RIGHT_PIN);
    ButtonConfigure(BUTTON_JOY_UP_GPIO, BUTTON_JOY_UP_PIN);
    ButtonConfigure(BUTTON_JOY_DOWN_GPIO, BUTTON_JOY_DOWN_PIN);
    ButtonConfigure(BUTTON_JOY_FIRE_GPIO, BUTTON_JOY_FIRE_PIN);

    ButtonConfigure(BUTTON_MODE_GPIO, BUTTON_MODE_PIN);
}

void ReadButtons() {
    char tmpStates[BUTTON_COUNT] = {
        READ_BUTTON_USER,
        READ_BUTTON_JOY_LEFT,
        READ_BUTTON_JOY_RIGHT,
        READ_BUTTON_JOY_UP,
        READ_BUTTON_JOY_DOWN,
        READ_BUTTON_JOY_FIRE,
        READ_BUTTON_MODE
    };
    
    for (int i = 0; i < BUTTON_COUNT; ++i) {
        // Chyba ktoś wcisnął guzik. Trzeba to sprawdzić!
        if (buttonClock[i] == 0 && buttonStates[i] != tmpStates[i])
            buttonClock[i] = 1;
        
        // Podbijamy licznik czasu
        if (buttonClock[i] > 0 && buttonClock[i] < BUTTON_READ_DELAY)
            ++buttonClock[i];
        
        // Ponawiamy sprawdzenie po określonym odstępie
        if (buttonClock[i] >= BUTTON_READ_DELAY) {
            if (buttonStates[i] != tmpStates[i]) {
                buttonStates[i] = tmpStates[i];
                
                WriteString(buttonNames[i]);
                if (buttonStates[i])
                    WriteString(" PRESSED\r\n");
                else
                    WriteString(" RELEASED\r\n");
            }
            
            buttonClock[i] = 0;
        }
    }
}
