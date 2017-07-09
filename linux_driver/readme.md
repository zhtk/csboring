# Sterownik Monter

Po wykryciu urządzenia znajdowana jest wolna komórka w tablicy deviceids
i data, a następnie wypełniana jest danymi urządzenia.

Dostęp do urządzenia odbywa się metodą round robin, po wykonaniu INSTRUCTION_LIMIT
instrukcji użytkownik jest wywłaszczany i kontekst ulega zmianie (po
uprzednim zapisaniu aktualnego stanu rejestrów). Poprawność instrukcji jest
sprawdzana przed wysłaniem do urządzenia. Wszystkie operacje na urządzeniu
są chronione przez muteksy i spinlocki.

Wszędzie gdzie się da korzystam z DMA (tj. przy tworzeniu obszaru roboczego
i przy wczytywaniu intrukcji poprzez blok wczytywania instrukcji), write
jest w miarę możliwości asynchroniczny. Użytkownik ma bezpośredni dostęp
do pamięci roboczej widzianej przez urządzenie podczas mmapa bez pośrednictwa
sterownika.

Wszystkie testy przechodzą (trwa to ok. 5 minut)
