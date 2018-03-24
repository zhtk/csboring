Kompilator Latte
================

Latte jest językiem podobnym do Java.

## Kompilacja

    ./latc_llvm program.lat

## Uruchamianie programu:

    llvm-link program.bc lib/runtime.bc -o program.bc
    lli program.bc

## Wykorzystane biblioteki i programy zewnętrzne

* LLVM 5

## Zaimplementowane elementy języka

* frontend obsługuje wszystkie rozszerzenia
* w backendzie zaimplementowana jest podstawowa wersja języka + obsługa tablic
