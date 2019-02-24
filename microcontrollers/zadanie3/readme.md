# Zadanie 3

Prototyp programu z zadania.

## Znane usterki

* Akcelerometr nigdy nie wykrywa ruchu, odczyt przyspieszenia z
  poszczególnych osi zawsze zwraca 0. Aby rozwiązać ten problem należy
  w rejestrze CTRL_REG1 urządzenia ustawić bit PD.
* Cykl licznika generującego sygnał PWM trwa aż 255 cykli procesora.
  Trzeba go wyskalować w taki sposób, by wartości odczytywane z
  akcelerometru były dobrze widoczne.

## Możliwe usprawnienia

* Przepisanie komunikacji I2C na przerwania.
* Wydzielenie konfiguracji do nagłówków.
* Wydzielenie modułu I2C do osobnego pliku.
