#ifndef _BUTTON_H_
#define _BUTTON_H_

#define BUTTON_COUNT 7
#define BUTTON_READ_DELAY 400

#define BUTTON_USER_GPIO      GPIOC
#define BUTTON_JOY_LEFT_GPIO  GPIOB
#define BUTTON_JOY_RIGHT_GPIO GPIOB
#define BUTTON_JOY_UP_GPIO    GPIOB
#define BUTTON_JOY_DOWN_GPIO  GPIOB
#define BUTTON_JOY_FIRE_GPIO  GPIOB
#define BUTTON_MODE_GPIO      GPIOA

#define BUTTON_USER_PIN      13
#define BUTTON_JOY_LEFT_PIN  3
#define BUTTON_JOY_RIGHT_PIN 4
#define BUTTON_JOY_UP_PIN    5
#define BUTTON_JOY_DOWN_PIN  6
#define BUTTON_JOY_FIRE_PIN  10
#define BUTTON_MODE_PIN      0

extern void EnableButtons();
extern void ReadButtons();

#endif
