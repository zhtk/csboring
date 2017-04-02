#include <stdbool.h>

// Kolejka do komunikacji muzeum z bankiem
static const int MUZEUM_BANK_Q = 607481;

// Kolejki do komunikacji muzeum z firmami
// MFQ port = 1: służy do ubiegania się o pozwolenie
// MFQ port = 2: służy do zgłaszania się po wypłatę należności za artefakty
// MFQ port = 3: zbiera raporty firm. Pierwszy kominukat oznacza liczbę firm.
// Muzeum odsyła odpowiedź na port = 2 * id_firmy + MUZEUM_FIRMA_OFFSET
// Należy wysyłać strukturę delegat_query
static const int MUZEUM_FIRMA_Q = 607482;

// Offset który służy do utworzenia gniazd do komunikacji z muzeum.
// Dodatkowo każdy delegat ma po 2 gniazda:
// parzyste do wysyłania i nieparzyste do odbierania komunikatów 
// od firmy
static const int MUZEUM_FIRMA_OFFSET = 10;

// Struktury do komunikacji z bankiem
struct bank_query
{
	// 1 - podaj stan konta -> bank
	// 2 - podaj stan konta -> muzeum
	// 3 - zabierz kwotę z konta -> bank
	// 4 - przelej kwotę na konto -> bank
	// 5 - kończ pracę i prześlij raporty
	// 7 - firma o Fid = account kończy działalność
	long type;
	
	int account;
	int value;
};

struct delegat_query
{
	// Port na który jest wysłane zapytanie
	long port;
	// Jeżeli skierowane do delegata:
	// 1 - żądanie prac na działce Teren[i]
	// 2 - symbol zbioru najwyższego niezbadanego poziomu na działce Teren[i]
	// 3 - raport ze zbiorem znalezisk na jednym polu Teren[i][j]
	// 4 - zwrot pozwolenia
	// Jeżeli do wynagradzacza: numer firmy
	int type;
	bool result;
	int arg1;
	// Tablica długości A + 1
	int arg2[];
};

// muzeum odsyła odpowiedź na port = 2 * pozw.id_firmy + MUZEUM_FIRMA_OFFSET
struct pozwolenie 
{
	long port;
	// Jeśli id_firmy = -1 to muzeum ma skończyć pracę
	int id_firmy;
	int pocz;
	int glebokosc;
	int pracownicy;
	int oplata;
	bool czy_ok;
};
