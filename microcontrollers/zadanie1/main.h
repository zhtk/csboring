#ifndef _MAIN_H_
#define _MAIN_H_

#define LED_COUNT 4

#define RED_LED_GPIO    GPIOA
#define GREEN_LED_GPIO  GPIOA
#define BLUE_LED_GPIO   GPIOB
#define GREEN2_LED_GPIO GPIOA

#define RED_LED_PIN    6
#define GREEN_LED_PIN  7
#define BLUE_LED_PIN   0
#define GREEN2_LED_PIN 5

extern int ExecuteLEDcommand(char *, int);

#endif

