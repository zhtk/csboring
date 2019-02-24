#include <stddef.h>

#include "sensors.h"
#include "i2c.h"

// Wartości temperatury
static char temperatureValue[2];

// Wartości ciśnienia
static char pressureStatusValue;
static char pressureReadValue[4];

// Wartości wilgotności
static char humidityInterpolationValues[6];
static char humidityStatusRegisterValue;
static char humidityReadValue[2];

// Wartości światła
static char lightReadValue[4];

// Funkcja czytająca wszystkie sensory
void ReadAllSensors(void) {
    __disable_irq();
    
    ReadPressureStatus();
    ReadHumidityStatus();
    ReadLightSensor();
    
    __enable_irq();
}

// Mało ważne callbacki
void PressureStatusReadCallback(int status) {
    ReadPressureValue();
}

void HumidityStatusReadCallback(int status) {
    ReadHumidityValue();
}

void ReadHumidityValueCallback(int status) {
    ReadTemperature();
}

// Funkcje czytające sensory
void ReadTemperature(void) {
    static char temperatureQuery[] = { STLM75_TEMP_REG };
    
    I2CwriteRead(STLM75_ADDR, temperatureQuery, sizeof(temperatureQuery),
        temperatureValue, sizeof(temperatureValue), TemperatureReadCallback);
}

float GetTemperature(void) {
    float temperature = (int8_t) temperatureValue[0];
    
    if (temperatureValue[1] & STLM75_TEMP_FLOAT_PART)
        temperature += 0.5f;
    
    return temperature;
}

void EnablePressureSensor(void) {
    static char pressureControlEnable[] = {
        LPS331AP_CTRL_REG1,
        LPS331AP_CTRL_REG1_SET
    };
    
    I2CwriteRead(LPS331AP_ADDR, pressureControlEnable,
        sizeof(pressureControlEnable), NULL, 0, NULL);
}

void ReadPressureStatus(void) {
    static char pressureStatusQuery[] = { LPS331AP_STATUS_REG };
    
    I2CwriteRead(LPS331AP_ADDR, pressureStatusQuery, sizeof(pressureStatusQuery),
        &pressureStatusValue, sizeof(pressureStatusValue), PressureStatusReadCallback);
}

int GetPressureStatusDataAvailable(void) {
    return pressureStatusValue & LPS331AP_STATUS_REG_P_DA;
}

void ReadPressureValue(void) {
    static char pressureReadRegisters[] = {
        LPS331AP_PRESS_OUT_XL,
        LPS331AP_PRESS_OUT_L,
        LPS331AP_PRESS_OUT_H
    };
    
    if (GetPressureStatusDataAvailable()) {
        // Pamiętamy o tym, że kolejność bajtów to little endian!
        I2CwriteRead(LPS331AP_ADDR, pressureReadRegisters + 0, 1,
            pressureReadValue + 0, 1, NULL);
        I2CwriteRead(LPS331AP_ADDR, pressureReadRegisters + 1, 1,
            pressureReadValue + 1, 1, NULL);
        I2CwriteRead(LPS331AP_ADDR, pressureReadRegisters + 2, 1,
            pressureReadValue + 2, 1, NULL);
    }
}

int GetPressure(void) {
    // Pout(mbar) = (PRESS_OUT_H & PRESS_OUT_L & PRESS_OUT_XL)[dec] / 4096
    return *((uint32_t*) pressureReadValue) / 4096;
}

void EnableHumiditySensor(void) {
    static char humidityControlRegister[] = {
        HTS221_CTRL_REG1,
        HTS221_CTRL_REG1_SET
    };
    
    static char humidityInterpolationRegisters[] = {
        HTS221_H0_RH_X2,
        HTS221_H1_RH_X2,
        HTS221_H0_T0_OUT_L,
        HTS221_H0_T0_OUT_H,
        HTS221_H1_T0_OUT_L,
        HTS221_H1_T0_OUT_H
    };
    
    I2CwriteRead(HTS221_ADDR, humidityControlRegister,
        sizeof(humidityControlRegister), NULL, 0, NULL);
    
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 0, 1,
        humidityInterpolationValues + 0, 1, NULL);
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 1, 1,
        humidityInterpolationValues + 1, 1, NULL);
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 2, 1,
        humidityInterpolationValues + 2, 1, NULL);
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 3, 1,
        humidityInterpolationValues + 3, 1, NULL);
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 4, 1,
        humidityInterpolationValues + 4, 1, NULL);
    I2CwriteRead(HTS221_ADDR, humidityInterpolationRegisters + 5, 1,
        humidityInterpolationValues + 5, 1, NULL);
}

void ReadHumidityStatus(void) {
    static char humidityStatusRegister[] = { HTS221_STATUS_REG };
    
    I2CwriteRead(HTS221_ADDR, humidityStatusRegister, sizeof(humidityStatusRegister),
        &humidityStatusRegisterValue, 1, HumidityStatusReadCallback);
}

int GetHumidityStatusDataAvailable(void) {
    return humidityStatusRegisterValue & HTS221_STATUS_REG_H_DA;
}

void ReadHumidityValue(void) {
    static char humidityReadRegisters[] = {
        HTS221_HUMIDITY_OUT_L,
        HTS221_HUMIDITY_OUT_H
    };
    
    if (GetHumidityStatusDataAvailable()) {
        I2CwriteRead(HTS221_ADDR, humidityReadRegisters + 0, 1,
            humidityReadValue + 0, 1, NULL);
        I2CwriteRead(HTS221_ADDR, humidityReadRegisters + 1, 1,
            humidityReadValue + 1, 1, ReadHumidityValueCallback);
    } else {
        ReadHumidityValueCallback(0);
    }
}

float GetHumidity(void) {
    float H0_OUT = *((int16_t *) (humidityInterpolationValues + 2));
    float H_OUT = *((int16_t *) humidityReadValue);
    float H1_OUT = *((int16_t *) (humidityInterpolationValues + 4));
    
    float relative = (H_OUT - H0_OUT) / (H1_OUT - H0_OUT);

    float result = (uint8_t) humidityInterpolationValues[1] / 2;
    result -= (uint8_t) humidityInterpolationValues[0] / 2;
    result *= relative;
    result += (uint8_t) humidityInterpolationValues[0] / 2;
    
    if (result < 0)
        result = 0;
    
    if (result > 100)
        result = 100;
    
    return result;
}

void EnableLightSensor(void) {
    static char lightControlEnable[] = {
        TSL2572_ENABLE_REG,
        TSL2572_ENABLE_REG_SET
    };
    
    I2CwriteRead(TSL2572_ADDR, lightControlEnable, sizeof(lightControlEnable),
        NULL, 0, EnableLightSensorCallback);
}

void ReadLightSensor(void) {
    static char lightReadRegister[] = { TSL2572_C0DATA_REG };
    
    I2CwriteRead(TSL2572_ADDR, lightReadRegister, sizeof(lightReadRegister),
        lightReadValue, sizeof(lightReadValue), NULL);
}

float GetLight(void) {
    int16_t c0data = *((int16_t *) lightReadValue);
    int16_t c1data = *((int16_t *) (lightReadValue + 2));
    
    // CPL = (ATIME_ms × AGAINx) / (GA × 60)
    // Lux1 = (1 × C0DATA − 1.87 × C1DATA) / CPL
    // Lux2 = (0.63 × C0DATA − 1 × C1DATA) / CPL
    float lux1 = (c0data - 1.87f * c1data) / TSL2572_CPL;
    float lux2 = (0.63f * c0data - c1data) / TSL2572_CPL;
    
    // Lux = MAX(Lux1, Lux2, 0)
    float result = 0;
    
    if (lux1 > result)
        result = lux1;
    
    if (lux2 > result)
        result = lux2;
    
    return result;
}
