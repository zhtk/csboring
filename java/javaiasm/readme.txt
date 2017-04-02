Java vs asembler x64 - instrukcja NOP
=====================================

Jedną z najważniejszych instrukcji implementowanych przez każdą 
szanującą się architekturę procesorów jest instrukcja nop. O jej znaczeniu
dla losów świata świadczy fakt, że twórcy JVM przewidzieli taką instrukcję
w maszynie wirtualnej Javy.

Przetestowałem szybkość wykonywania tej instrukcji w JVM i na procesorze
intel core i7-2630QM będącego pod kontrolą systemu Linux 3.19.3-3-ARCH x86_64

Do kompilacji kodu asemblerowego użyłem programu NASM version 2.11.08 a
do kompilacji bajtkodu javy kompilatora jasmin-2.4 
[http://jasmin.sourceforge.net/] gdyż język Java nie pozwala na użycie 
instrukcji NOP w kodzie.

Szybkość programu w asemblerze
3,95s user 0,00s system 99% cpu 3,958 total
opcje kompilatora: nasm -f elf64

I w javie
4,07s user 0,01s system 100% cpu 4,062 total
opcje: java -jar jasmin.jar javaprog.jasm

Jak widać program w asemblerze szybciej wykonał 10 000 000 000 instrukcji 
NOP. Chciałbym jednak zauważyć że w tym czasie maszyna wirtualna javy 
wykonała dodatkowo weryfikację bajtkodu (prześledzenie stosu i weryfikacja
typów zmiennych), zaś sama specyfikacja bajtkodu JVM wymusza nieefektywne
rozwiązania:
- argumenty instrukcji JVM są przekazywane przez stos a nie przez rejestry
- po wywołaniu instrukcji argumenty są usuwane ze stosu (mimo iż wykorzystuję
  je wiele razy!) przez co muszę je każdorazowo zapisywać do rejestru i 
  zaraz potem przywracać na stosie
- instrukcje skoku warunkowego ("skocz jeśli argument na stosie jest równy 0")
  oczekują liczb typu int. Użycie innego typu jak short albo long wymusza 
  zastosowanie dodatkowej instrukcji cmp. Tymczasem procesorowi nie robi 
  to większej różnicy (bo używa flag).
W efekcie zamiast 3 instrukcji w pętli program javowy wykorzystuje aż 9 
(dodatkowe instrukcje to niepotrzebne porównanie i żonglerka między stosem 
a rejestrami). W związku z tymi faktami tak mała różnica między programem
w javie i asemblerze przemawia raczej na korzyść Javy.
