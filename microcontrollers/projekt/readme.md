# Pogodynka

Projekt polegał na stworzeniu mikrokontrolera, który
odczytuje temperaturę, wilgotność, ciśnienie i natężenie
światła, a następnie wyświetla odczytane wartości na
wyświetlaczu LCD.

## Sprzęt

* Płytka ewaluacyjna STM32 Nucleo z chipem STM32F4xx.
* Shield Kamami zawierający sensory: STLM75, LPS331AP, HTS221, TSL2572.
* Wyświetlacz LCD ST7735S.

## Komunikacja

* Komunikacja z sensorami odbywa się po interfejsie I2C.
* Komunikacja I2C jest oparta na przerwaniach.
* Wyświetlanie danych na LCD jest synchroniczne.

## Ocenianie

* Projekt oceniony na 5.
* Wyświetlanie danych na LCD powinno odbywać się z głównego wątku,
  a nie przerwania.

