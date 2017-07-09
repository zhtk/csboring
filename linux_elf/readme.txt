# Interceptor - opis rozwiązania

## Sposób działania
 * Iterujemy po wszystkich nagłówkach typu dynamic
 * Zbieramy z nagłówka informacje o tablicach stringów, symboli i
   dynamicznych relokacji
 * Przechodzimy po tablicy relokacji i lokalizujemy każdy jej element w
   tablicy symboli. Sprawdzamy, czy relokowany obiekt jest funkcją i
   czy rodzaj relokacji jest zgodny z R_X86_64_JUMP_SLOT. Jeśli tak, to
   sprawdzamy w tablicy stringów czy nazwa funkcji zgadza się z przechwytywaną
   funkcją
 * Podmieniamy adres funkcji w GOT
 * Stary adres zapisujemy w linkowanej liście, jeśli alokacja elementu się
   nie uda no to mamy pecha...

## Ograniczenia
 * biblioteka nie jest thread-safe, należy zapewnić odpowiednią
   synchronizację przy wołaniu jej funkcji
 * metadane przechwyconych funkcji są zapisywane na liście, z której później
   nigdy nie są usuwane
 * zakładam, że stringi z nazwami funkcji przekazywane przy wywołaniu
   funkcji biblioteki nie są nigdy dealokowane

## TODO
 * Wołanie linkera
