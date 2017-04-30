#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <time.h>

using namespace std;

// Ustawienia pliku, który będzie testowany
const int rozmiar_bloku = 8 * 1024; // 8 KB
const int bloki = 1024 * 10; // plik z danymi o rozmiarze 80 MB
string plik = "test.dat"; // ścieżka do pliku

// Ustawienia testowania
const int ile_odczytow = 1000000; // Ile odczytów (w blokach)
const int ile_zapisow  = 10000;   // Ile zapisów
const int max_operacji = 10; // Ile bloków maksymalnie można odczytać

// Zmienne globalne
void *bufor = NULL; // Bufor wyrównanej pamięci o rozmiarze bloku

// Wypełnia bufor losowymi danymi
void losuj_bufor(int ile = 1)
{
    int *tab = (int *) bufor;
    for (int i = 0; i < rozmiar_bloku * ile / sizeof(int); ++i)
        tab[i] = rand();
}

// Tworzy plik z danymi testowymi, na nim będą prowadzone operacje
void generuj_dane()
{
    int fd = open(plik.c_str(), O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
    
    for (int i = 0; i < bloki; ++i)    {
        losuj_bufor();
        write(fd, bufor, rozmiar_bloku);
    }
    
    close(fd);
}

void test(int fd, int odczyty, int zapisy, int ile = 1)
{    
    printf(" Ustawienia testu: %d odczytow %d zapisow %d blokow na raz\n", 
           odczyty, zapisy, ile);
    
    posix_memalign(&bufor, rozmiar_bloku, rozmiar_bloku * ile);
    
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    
    while (odczyty + zapisy > 0) {        
        int blok = rand() % (bloki - ile + 1);
        int operacja = rand() % (odczyty + zapisy);
        
        if (operacja < odczyty) {
            lseek(fd, blok * rozmiar_bloku, 0);
            read(fd, bufor, rozmiar_bloku * ile);
            
            odczyty -= ile;
        } else {
            //generuj_dane(ile); // Rzecz dyskusyjna
            
            lseek(fd, blok * rozmiar_bloku, 0);
            write(fd, bufor, rozmiar_bloku * ile);

            zapisy -= ile;
        }
    }
    
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);
    uint64_t delta = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_nsec - start.tv_nsec) / 1000;
    
    printf(" Czas testu: %u\n", delta);
    
    free(bufor);
}

int main()
{
    // Inicjalizacja
    srand(time(0));
    posix_memalign(&bufor, rozmiar_bloku, rozmiar_bloku);
    
    printf("Generowanie danych testowych\n");
    generuj_dane();
    free(bufor);
    
    // Testy operacji jednoblokowych
    printf("1B - Test z buforowaniem systemu plików\n");
    int fd = open(plik.c_str(), O_RDWR);
    test(fd, ile_odczytow, ile_zapisow);
    close(fd);
    
    printf("1B - Test z bezpośrednim dostępem do dysku\n");
    fd = open(plik.c_str(), O_RDWR | O_DIRECT);
    test(fd, ile_odczytow / 1000, ile_zapisow / 1000);
    close(fd);
    
    // Testy operacji wieloblokowych
    printf("WB - Test z buforowaniem systemu plików\n");
    fd = open(plik.c_str(), O_RDWR);
    test(fd, ile_odczytow, ile_zapisow, max_operacji);
    close(fd);
    
    printf("WB - Test z bezpośrednim dostępem do dysku\n");
    fd = open(plik.c_str(), O_RDWR | O_DIRECT);
    test(fd, ile_odczytow / 1000, ile_zapisow / 1000, max_operacji);
    close(fd);
    
    // Koniec pracy
    bufor = NULL;
}
