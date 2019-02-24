#ifndef _SENSORS_H_
#define _SENSORS_H_

#define STLM75_ADDR               0x48
#define STLM75_TEMP_REG           0x00
#define STLM75_TEMP_FLOAT_PART    0x80

#define LPS331AP_ADDR             0x5D
#define LPS331AP_CTRL_REG1        0x20
#define LPS331AP_CTRL_REG1_PD     0x80
#define LPS331AP_CTRL_REG1_ODR    0x20
#define LPS331AP_CTRL_REG1_BDU    0x04
#define LPS331AP_STATUS_REG       0x27
#define LPS331AP_STATUS_REG_P_DA  0x02
#define LPS331AP_PRESS_OUT_XL     0x28
#define LPS331AP_PRESS_OUT_L      0x29
#define LPS331AP_PRESS_OUT_H      0x2A
#define LPS331AP_CTRL_REG1_SET (LPS331AP_CTRL_REG1_PD | \
    LPS331AP_CTRL_REG1_ODR | LPS331AP_CTRL_REG1_BDU)

#define HTS221_ADDR               0x5F
#define HTS221_CTRL_REG1          0x20
#define HTS221_STATUS_REG         0x27
#define HTS221_STATUS_REG_H_DA    0x02
#define HTS221_HUMIDITY_OUT_L     0x28
#define HTS221_HUMIDITY_OUT_H     0x29
#define HTS221_H0_RH_X2           0x30
#define HTS221_H1_RH_X2           0x31
#define HTS221_H0_T0_OUT_L        0x36
#define HTS221_H0_T0_OUT_H        0x37
#define HTS221_H1_T0_OUT_L        0x3A
#define HTS221_H1_T0_OUT_H        0x3B
#define HTS221_CTRL_REG1_PD       0x80
#define HTS221_CTRL_REG1_BDU      0x04
#define HTS221_CTRL_REG1_ODR      0x02
#define HTS221_CTRL_REG1_SET      (HTS221_CTRL_REG1_PD | HTS221_CTRL_REG1_BDU | \
    HTS221_CTRL_REG1_ODR)

#define TSL2572_ADDR              0x39
#define TSL2572_COMMAND_REG       0xA0
#define TSL2572_ENABLE_REG        (0x00 | TSL2572_COMMAND_REG)
#define TSL2572_C0DATA_REG        (0x14 | TSL2572_COMMAND_REG)
#define TSL2572_ENABLE_REG_SET    0x03
#define TSL2572_AGAINX_VALUE      1
#define TSL2572_ATIME_VALUE       2.73f
#define TSL2572_GLASS_ATTENUATION 1
#define TSL2572_CPL ((TSL2572_ATIME_VALUE * TSL2572_AGAINX_VALUE) / \
    (TSL2572_GLASS_ATTENUATION * 60))

void ReadAllSensors(void);

void ReadTemperature(void);
void TemperatureReadCallback(int status);
float GetTemperature(void);

void EnablePressureSensor(void);
void ReadPressureStatus(void);
void PressureStatusReadCallback(int status);
int GetPressureStatusDataAvailable(void);
void ReadPressureValue(void);
int GetPressure(void);

void EnableHumiditySensor(void);
void ReadHumidityStatus(void);
void HumidityStatusReadCallback(int status);
int GetHumidityStatusDataAvailable(void);
void ReadHumidityValue(void);
void ReadHumidityValueCallback(int status);
float GetHumidity(void);

void EnableLightSensor(void);
void EnableLightSensorCallback(int status);
void ReadLightSensor(void);
float GetLight(void);

#endif
