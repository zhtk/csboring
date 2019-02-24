#include <delay.h>
#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "buttons.h"
#include "usart.h"

int main() {
    __disable_irq();

    EnableUSART2();
    EnableButtons();

    WriteString("DEVICE READY\r\n");
    __enable_irq();

    for (;;);
}
