#ifndef _I2C_H_
#define _I2C_H_

#include <stm32.h>

#define I2C_SPEED_HZ  100000
#define PCLK1_MHZ     16

#define I2C_SCL_GPIO  GPIOB
#define I2C_SDA_GPIO  GPIOB

#define I2C_SCL_PIN   8
#define I2C_SDA_PIN   9

#define I2C_TASK_BUFFER 50
#define I2C_IRQ_PRIORITY 5U

#define I2C_ERRORS (I2C_SR1_BERR | I2C_SR1_ARLO | I2C_SR1_AF | \
                    I2C_SR1_OVR | I2C_SR1_PECERR | I2C_SR1_TIMEOUT | \
                    I2C_SR1_SMBALERT)

void EnableI2C(void);

int I2CwriteRead(char address, char *sendBuffer, int sendlength,
	char *receiveBuffer, int readlength, void (*callback) (int));

int I2CwriteReadGuarded(char address, char *sendBuffer, int sendlength,
	char *receiveBuffer, int readlength, void (*callback) (int));

#endif
