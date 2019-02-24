#include <delay.h>
#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "main.h"
#include "buttons.h"
#include "usart.h"

typedef struct {
    void (*LEDon)();
    void (*LEDoff)();
    uint8_t state;
} LedController;

void RedLEDon()     { RED_LED_GPIO->BSRRH = 1 << RED_LED_PIN; }
void RedLEDoff()    { RED_LED_GPIO->BSRRL = 1 << RED_LED_PIN; }
void GreenLEDon()   { GREEN_LED_GPIO->BSRRH = 1 << GREEN_LED_PIN; }
void GreenLEDoff()  { GREEN_LED_GPIO->BSRRL = 1 << GREEN_LED_PIN; }
void BlueLEDon()    { BLUE_LED_GPIO->BSRRH = 1 << BLUE_LED_PIN; }
void BlueLEDoff()   { BLUE_LED_GPIO->BSRRL = 1 << BLUE_LED_PIN; }
void Green2LEDon()  { GREEN2_LED_GPIO->BSRRL = 1 << GREEN2_LED_PIN; }
void Green2LEDoff() { GREEN2_LED_GPIO->BSRRH = 1 << GREEN2_LED_PIN; }

LedController ledControl[LED_COUNT] = {
    {RedLEDon,    RedLEDoff,    0},
    {GreenLEDon,  GreenLEDoff,  0},
    {BlueLEDon,   BlueLEDoff,   0},
    {Green2LEDon, Green2LEDoff, 0},
};

void EnableLEDs() {
    RedLEDoff();
    GreenLEDoff();
    BlueLEDoff();
    Green2LEDoff();
    
    GPIOoutConfigure(RED_LED_GPIO, RED_LED_PIN, GPIO_OType_PP,
        GPIO_Low_Speed, GPIO_PuPd_NOPULL);
    GPIOoutConfigure(GREEN_LED_GPIO, GREEN_LED_PIN, GPIO_OType_PP,
        GPIO_Low_Speed, GPIO_PuPd_NOPULL);
    GPIOoutConfigure(BLUE_LED_GPIO, BLUE_LED_PIN, GPIO_OType_PP,
        GPIO_Low_Speed, GPIO_PuPd_NOPULL);

    GPIOoutConfigure(GREEN2_LED_GPIO, GREEN2_LED_PIN, GPIO_OType_PP,
        GPIO_Low_Speed, GPIO_PuPd_NOPULL);
}

/*
 *  Format polecenia:
 *  LED[nr][operacja]
 * 
 *  nr = 1, 2, 3, 4
 *  operacja = N (on), F (off), T (toggle)
 * 
 *  np.: LED1N
 */
int ExecuteLEDcommand(char *cmd, int cmdlen) {
    if (cmdlen < 5) {
        return 1;
    }

    if (strncmp(cmd, "LED", strlen("LED"))) {
        WriteString("NOT A LED COMMAND\r\n");
        return 0;
    }
    
    uint8_t led = cmd[3] - '0' - 1;
    char command = cmd[4];
    
    if (led < 0 || led >= LED_COUNT) {
        WriteString("INVALID LED NUMBER\r\n");
        return 0;
    }

    WriteString("EXECUTE LED COMMAND\r\n");

    switch (command) {
        case 'N':
            ledControl[led].state = 1;
            ledControl[led].LEDon();
            break;
        case 'F':
            ledControl[led].state = 0;
            ledControl[led].LEDoff();
            break;
        case 'T':
            if (ledControl[led].state) {
                ledControl[led].LEDoff();
            } else {
                ledControl[led].LEDon();
            }

            ledControl[led].state = !ledControl[led].state;
            break;
    }
    
    return 0;
}

int main() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_GPIOBEN | RCC_AHB1ENR_GPIOCEN;
    __NOP();
    
    EnableLEDs();
    EnableUSART2();
    EnableButtons();
    
    WriteString("DEVICE READY\r\n");

    for (;;) {
        ReadButtons();
        ProcessReadBuffer();
        ProcessWriteBuffer();
    }
}
