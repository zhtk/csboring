#include <delay.h>
#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "i2c.h"
#include "usart.h"

static volatile int I2CtasksCurrent = 0;
static volatile int I2CtasksLast = 0;

static struct I2Ctask {
    char *input;
    char *output;
    void (*callback) (int);
    int input_length;
    int output_length;
    char address;
} I2Ctasks[I2C_TASK_BUFFER];

static inline int I2CnextFreeTaskSlot(void) {
    return (I2CtasksLast + 1) % I2C_TASK_BUFFER;
}

static inline int HasI2CfreeTaskSlot(void) {
    return I2CnextFreeTaskSlot() != I2CtasksCurrent;
}

static inline void I2CtaskFinishedCallback(struct I2Ctask *task, int result) {
    if (task->callback != NULL)
        task->callback(result);
}

static inline void I2CpickNextTask(void) {
    // Pobranie kolejnego zadania.
    I2CtasksCurrent = (I2CtasksCurrent + 1) % I2C_TASK_BUFFER;
}

static inline void I2CsendEndingSignal(void) {
    if (I2CtasksCurrent == I2CtasksLast) {
        // Zwolnienie szyny danych.
        I2C1->CR1 |= I2C_CR1_STOP;
    } else {
        // Repeated start.
        I2C1->CR1 |= I2C_CR1_START;
    }
}

void EnableI2C(void) {
    // Aktywacja niezbędnych szyn
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOBEN;
    RCC->APB1ENR |= RCC_APB1ENR_I2C1EN;
    
    // Konfiguracja pinów
    GPIOafConfigure(I2C_SCL_GPIO, I2C_SCL_PIN, GPIO_OType_OD, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_I2C1);
    GPIOafConfigure(I2C_SDA_GPIO, I2C_SDA_PIN, GPIO_OType_OD, GPIO_Low_Speed,
        GPIO_PuPd_NOPULL, GPIO_AF_I2C1);

    // Wyczyszczenie rejestru kontrolnego I2C
    I2C1->CR1 = 0;
    
    // Ustawienie parametrów transmisji
    I2C1->CR2 = PCLK1_MHZ;
    I2C1->CCR = (PCLK1_MHZ * 1000000) /    (I2C_SPEED_HZ << 1);
    I2C1->TRISE = PCLK1_MHZ + 1;
    
    // Włączenie interfejsu
    I2C1->CR1 |= I2C_CR1_PE;

    // Konfiguracja przerwań I2C
    NVIC_SetPriority(I2C1_EV_IRQn, I2C_IRQ_PRIORITY);
    NVIC_SetPriority(I2C1_ER_IRQn, I2C_IRQ_PRIORITY);
    
    NVIC_EnableIRQ(I2C1_EV_IRQn);
    NVIC_EnableIRQ(I2C1_ER_IRQn);
    
    I2C1->CR2 |= I2C_CR2_ITEVTEN | I2C_CR2_ITERREN;
}

static void HandleI2Cerrors(struct I2Ctask *task, int interrupts) {
    if (!(interrupts & I2C_ERRORS))
        return;
    
    char *error = "UNKNOWN ERROR\r\n";
    
    if (interrupts & I2C_SR1_BERR)
        error = "I2C BERR\r\n";
    
    if (interrupts & I2C_SR1_ARLO)
        error = "I2C ARLO\r\n";
    
    if (interrupts & I2C_SR1_AF)
        error = "I2C AF\r\n";
    
    if (interrupts & I2C_SR1_OVR)
        error = "I2C OVR\r\n";
    
    if (interrupts & I2C_SR1_PECERR)
        error = "I2C PECERR\r\n";
    
    if (interrupts & I2C_SR1_TIMEOUT)
        error = "I2C TIMEOUT\r\n";
    
    if (interrupts & I2C_SR1_SMBALERT)
        error = "I2C SMBALERT\r\n";
    
    I2CtaskFinishedCallback(task, 0);
    I2CpickNextTask();
    I2CsendEndingSignal();
    
    WriteString(error);
}

static void SendDataHandler(struct I2Ctask *task, int interrupts) {
    // Transmisja zainicjowana. Należy wysłać adres urządzenia. Tryb MT.
    if (interrupts & I2C_SR1_SB) {
        I2C1->DR = task->address << 1;
        
        WriteString("I2C SEND SB\r\n");
    }
        
    // Adres urządzenia przesłany. Aktywacja przerwań związanych
    // z buforem nadawczym.
    if (interrupts & I2C_SR1_ADDR) {
        I2C1->SR2;
        I2C1->CR2 |= I2C_CR2_ITBUFEN;
        
        WriteString("I2C SEND ADDR\r\n");
    }
    
    // Dopóki nie wysłaliśmy ostatniego bajtu nie przejmujemy się
    // zdarzeniem BTF. Kolejne bajty wstawiamy do kolejki gdy nastąpi TXE.
    if (task->input_length > 0 && interrupts & I2C_SR1_TXE) {
        I2C1->DR = *(task->input);

        ++task->input;
        --task->input_length;
        
        WriteString("I2C SEND TXE\r\n");
    }
}

static void AfterSendHandler(struct I2Ctask *task, int interrupts) {
    // Kod wspólny dla przypadku gdy przyszło TXE lub BTF.
    // Wysyłanie zakończone, więc nie musimy już czekać na TXE.
    I2C1->CR2 &= ~I2C_CR2_ITBUFEN;
    
    // Jeżeli nie ma więcej bajtów do wysłania to czekamy na BTF.
    // O ile będziemy chcieli coś za chwilę odebrać. Wtedy zrobimy
    // repeated start.
    if (task->output_length && interrupts & I2C_SR1_BTF) {
        // Repeated start na potrzeby odbierania danych.
        I2C1->CR1 |= I2C_CR1_START;
        
        // Oznaczenie tej fazy zadania jako zakończona.
        task->input_length = -1;
        
        WriteString("I2C AFTER SEND BTF RPT\r\n");
    }
    
    // Jeżeli po fazie wysyłania nie ma nic do zrobienia to kończymy
    // przetwarzanie zadania.
    if (task->output_length == 0 && interrupts & I2C_SR1_BTF) {
        I2CtaskFinishedCallback(task, 1);
        I2CpickNextTask();
        I2CsendEndingSignal();
        
        WriteString("I2C AFTER SEND BTF FINISHED\r\n");
    }
}

static void ReceiveDataHandler(struct I2Ctask *task, int interrupts) {
    // Transmisja zainicjowana. Należy wysłać adres urządzenia. Tryb MR.
    if (interrupts & I2C_SR1_SB) {
        I2C1->DR = task->address << 1 | 1U;
        
        if (task->output_length > 1) {
            I2C1->CR1 |= I2C_CR1_ACK;
            
            WriteString("I2C RECEIVE SB ACK\r\n");
        } else {
            I2C1->CR1 &= ~I2C_CR1_ACK;
            
            WriteString("I2C RECEIVE SB NACK\r\n");
        }
    }
        
    // Adres wysłany. Należy przygotować się do odbierania danych.
    if (interrupts & I2C_SR1_ADDR) {
        I2C1->CR2 |= I2C_CR2_ITBUFEN;
        I2C1->SR2;
        
        if (task->output_length <= 1)
            I2C1->CR1 |= I2C_CR1_STOP;
        
        WriteString("I2C RECEIVE ADDR\r\n");
    }
    
    // Faza odbierania danych, o ile został więcej niż 1 bajt.
    if (task->output_length > 2 && interrupts & I2C_SR1_RXNE) {
        *(task->output) = I2C1->DR;
        
        --task->output_length;
        ++task->output;
        
        WriteString("I2C RECEIVE RXNE > 2\r\n");
        return;
    }
    
    // Faza odbierania danych, o ile został do przesłania dokładnie 1 bajt.
    if (task->output_length == 2 &&    interrupts & I2C_SR1_RXNE) {
        *(task->output) = I2C1->DR;
        
        --task->output_length;
        ++task->output;
        
        // Ustalenie sygnałów do nadania po odebraniu ostatniego bajtu.
        I2C1->CR1 &= ~I2C_CR1_ACK;
        I2C1->CR1 |= I2C_CR1_STOP;
        
        WriteString("I2C RECEIVE RXNE == 2\r\n");
        return;
    }
    
    // Odebranie ostatniego bajtu i przeskoczenie do kolejnego zadania.
    if (task->output_length == 1 &&    interrupts & I2C_SR1_RXNE) {
        *(task->output) = I2C1->DR;
        
        --task->output_length;
        ++task->output;
        
        I2C1->CR2 &= ~I2C_CR2_ITBUFEN;
        
        I2CtaskFinishedCallback(task, 1);
        I2CpickNextTask();
        
        if (I2CtasksCurrent != I2CtasksLast)
            I2C1->CR1 |= I2C_CR1_START;
        
        WriteString("I2C RECEIVE RXNE == 1 FINISHED\r\n");
    }
}

static void I2Chandler(void) {
    // Odebranie przerwań
    int interrupts = I2C1->SR1;

    struct I2Ctask *task = I2Ctasks + I2CtasksCurrent;

    // Faza nadawania danych.
    if (task->input_length > 0)
        SendDataHandler(task, interrupts);
    // Oczekiwanie na koniec nadawania
    else if (task->input_length == 0 && task->output_length >= 0)
        AfterSendHandler(task, interrupts);
    // Faza odbierania danych
    else if (task->input_length == -1 && task->output_length > 0)
        ReceiveDataHandler(task, interrupts);
    
    // Obsługa błędów
    HandleI2Cerrors(task, interrupts);
}

void I2C1_EV_IRQHandler(void) {
    I2Chandler();
}

void I2C1_ER_IRQHandler(void) {
    I2Chandler();
}

int I2CwriteRead(char address, char *sendBuffer, int sendlength,
    char *receiveBuffer, int readlength, void (*callback) (int)) {
    // Sprawdź, czy w buforze cyklicznym I2Ctask jest wolny slot na zadanie
    if (!HasI2CfreeTaskSlot())
        return 0;

    // Poprawienie sendlength do formatu oczekiwanego przez funkcję wysyłającą
    if (sendlength == 0)
        sendlength = -1;

    // Skopiuj zadanie do kolejki
    I2Ctasks[I2CtasksLast].address = address;
    I2Ctasks[I2CtasksLast].input = sendBuffer;
    I2Ctasks[I2CtasksLast].input_length = sendlength;
    I2Ctasks[I2CtasksLast].output = receiveBuffer;
    I2Ctasks[I2CtasksLast].output_length = readlength;
    I2Ctasks[I2CtasksLast].callback = callback;

    // Jeśli w kolejce nie było żadnego taska to zainicjuj transmisję
    if (I2CtasksCurrent == I2CtasksLast)
        I2C1->CR1 |= I2C_CR1_START;

    // Podbicie wskaźnika następnego wolnego slotu na zadanie
    I2CtasksLast = I2CnextFreeTaskSlot();

    return 1;
}

int I2CwriteReadGuarded(char address, char *sendBuffer, int sendlength,
    char *receiveBuffer, int readlength, void (*callback) (int)) {
    __disable_irq();
    
    int result = I2CwriteRead(address, sendBuffer, sendlength,
        receiveBuffer, readlength, callback);
    
    __enable_irq();
    
    return result;
}
