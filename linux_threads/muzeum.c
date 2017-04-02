#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <pthread.h>

#include "config.h"

// Sposób na irytujące warningi
#define free(ptr) free((void *) ptr)

// Stan:
// 0, jeśli pole Teren[i][j] zostało oczyszczone,
// 1, jeśli pole Teren[i][j] nie zostało oczyszczone ale jest zarezerwowane,
// 2, wpp.
volatile int **teren, **szacunek, **stan;
// S - stała opłata za pozwolenie
// A - maksymalny numer artefaktu
// dlugosc, glebokosc - rozmiar planszy
int dlugosc, glebokosc, S, A;
// Mutex blokujący dostęp do tablicy
pthread_mutex_t mutex, msgmut;
// Ewidencja posiadanych artefaktów
volatile int *ewidencja;
// Pola do wydobycia
volatile int do_wydobycia;

// Wczytuje mapę
void wczytaj_mape()
{
	teren = calloc(dlugosc, sizeof(int*));
	szacunek = calloc(dlugosc, sizeof(int*));
	stan = calloc(dlugosc, sizeof(int*));

	for(int i = 0; i < dlugosc; ++i) {
		teren[i] = calloc(glebokosc, sizeof(int));
		szacunek[i] = calloc(glebokosc, sizeof(int));
		stan[i] = calloc(glebokosc, sizeof(int));

		for(int j = 0; j < glebokosc; ++j) {
			stan[i][j] = 2;
			scanf("%d", teren[i] + j);
		}
	}

	for(int i = 0; i < dlugosc; ++i)
		for(int j = 0; j < glebokosc; ++j)
			scanf("%d", szacunek[i] + j);
}

// Wypisuje stan wykopalisk
void wypisz_stan()
{
	for(int i = 0; i < dlugosc; ++i) {
		for(int j = 0; j < glebokosc; ++j)
			printf("%d ", stan[i][j]);
		printf("\n");
	}
}

// Szuka odpowiedniego fragmentu działki by przydzielić pozwolenie
// Przed użyciem należy zablokować tablice mutexem
bool szukaj_pozwolenia(int pracownicy, int oplata, int *p, int *g)
{
	oplata -= S;
	
	for(*p = 0; *p < dlugosc - pracownicy + 1; ++(*p)) {
		int wart = 0;
		
		for(*g = 0; *g < glebokosc; ++(*g)) {
			bool res = false;
			
			for(int i = *p; i < *p + pracownicy; ++i) {
				wart += szacunek[i][*g];
				
				if(stan[i][*g] == 1)
					res = true;
			}
			
			if(res || wart > oplata)
				break;
				
			if(wart && wart <= oplata) 
				return true;
		}
	}
	
	return false;
}

void* wynagradzacz(void *arg)
{
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	int mfq = msgget(MUZEUM_FIRMA_Q, 0660);
	
	for(;;) {
		struct delegat_query dq;
		int ret = msgrcv(mfq, &dq, sizeof(struct delegat_query), 2, 0);
		
		if(ret < 0)
			break;
		
		ewidencja[dq.arg1] += dq.arg1;
		
		struct bank_query bank_q;
		bank_q.type = 4;
		bank_q.account = dq.type;
		bank_q.value = dq.arg1 * 10;
		
		msgsnd(mbq, &bank_q, sizeof(struct bank_query), 0);
	}
	
	return NULL;
}

void wypisz_raporty()
{
	int mid = msgget(MUZEUM_BANK_Q, 0660);
	
	// Wysłanie wiadomości do banku
	struct bank_query bank_q;
	bank_q.type = 5;
	msgsnd(mid, &bank_q, sizeof(struct bank_query), 0);
	
	// Wypisanie raportów
	int ile_firm;
	
	struct delegat_query *dq;
	dq = malloc(sizeof(struct delegat_query) + sizeof(int)*(A+1));
	msgrcv(mid, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 3, 0);
	ile_firm = dq->arg1;
	
	while(ile_firm--) {
		msgrcv(mid, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 3, 0);
		
		printf("%d %d\n", dq->type, dq->arg1);
		
		for(int i = 0; i <= A; ++i)
			if(dq->arg2[i] != 0)
				printf("%d %d\n", i, dq->arg2[i]);
		
		printf("\n");
	}
	
	free(dq);
}

// Koniec totalny działalności muzeum
void koniec_dzialalnosci(int s)
{
	pthread_mutex_lock(&mutex);
	pthread_mutex_lock(&msgmut);
	
	// Raport kończący
	wypisz_stan();

	// Zablokowanie delegatów i raporty firm
	wypisz_raporty();
	
	// Usunięcie kolejek
	int mid = msgget(MUZEUM_BANK_Q, 0660);
	if (msgctl(mid, IPC_RMID, 0) == -1)
		fprintf(stderr, "SIGINT - MUZEUM_BANK_Q - msgctl RMID error\n");

	mid = msgget(MUZEUM_FIRMA_Q, 0660);
	if (msgctl(mid, IPC_RMID, 0) == -1)
		fprintf(stderr, "SIGINT - MUZEUM_FIRMA_Q - msgctl RMID error\n");

	// Zwolnienie pamięci
	for(int i = 0; i < dlugosc; ++i) {
		free(teren[i]);
		free(szacunek[i]);
		free(stan[i]);
	}

	free(stan);
	free(szacunek);
	free(teren);
	free(ewidencja);

	pthread_mutex_destroy(&mutex);
	pthread_mutex_destroy(&msgmut);

	// Koniec procesu
	exit(0);
}

// Przygotowuje działalność muzeum
void przygotuj()
{
	do_wydobycia = dlugosc * glebokosc;
	// Inicjowanie mutexa
	pthread_mutex_init(&mutex, NULL);
	pthread_mutex_init(&msgmut, NULL);
	
	// Kolejka dla banku
	if (msgget(MUZEUM_BANK_Q, IPC_CREAT | IPC_EXCL | 0660) == -1) {
		int mid = msgget(MUZEUM_BANK_Q, 0660);
		if (msgctl(mid, IPC_RMID, 0) == -1)
			fprintf(stderr, "MUZEUM_BANK_Q - msgctl RMID error\n");
		fprintf(stderr, "MUZEUM_BANK_Q - creation error - try again\n");
		exit(1);
	}

	// Kolejka dla firm
	if (msgget(MUZEUM_FIRMA_Q, IPC_CREAT | IPC_EXCL | 0660) == -1) {
		int mid = msgget(MUZEUM_FIRMA_Q, 0660);
		if (msgctl(mid, IPC_RMID, 0) == -1)
			fprintf(stderr, "MUZEUM_FIRMA_Q - msgctl RMID error\n");
		fprintf(stderr, "MUZEUM_FIRMA_Q - creation error - try again\n");
		exit(1);
	}
	
	// Wątek do wypłaty wynagrodzenia
	pthread_t th;
	pthread_attr_t pat;
	pthread_attr_init(&pat);
	pthread_attr_setdetachstate(&pat, PTHREAD_CREATE_DETACHED);
	int ret = pthread_create(&th, &pat, wynagradzacz, NULL);
	pthread_attr_destroy(&pat);
	
	if(ret < 0) {
		fprintf(stderr, "pthread_create(wynagradzacz) - error\n");
		exit(1);
	}
	
	// Ustawia obsługę SIGINTa
	struct sigaction sa;
	sa.sa_handler = koniec_dzialalnosci;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_RESETHAND;
	sigaction(SIGINT, &sa, NULL);
	
	// Wczytanie terenu
	ewidencja = calloc(A + 1, sizeof(int));
	for(int i = 0; i <= A; ++i)
		ewidencja[i] = 0;
		
	wczytaj_mape();
}

void* delegat(void *arg)
{
	struct pozwolenie p = *((struct pozwolenie *) arg);
	free(arg);
	
	int wolne_pola = p.pracownicy * (p.glebokosc + 1);
	int mfq = msgget(MUZEUM_FIRMA_Q, 0660);
	
	// Na jakim poziomie są prowadzone prace lub -1 jeśli nie są
	int dzialki[p.pracownicy];
	for(int i = 0; i < p.pracownicy; ++i)
		dzialki[i] = -1;
	
	while(wolne_pola) {
		struct delegat_query *dq;
		dq = malloc(sizeof(struct delegat_query) + sizeof(int)*(A+1));
		int ret = msgrcv(mfq, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), p.port + 1, 0);
		(*dq).port = p.port;
		
		if(ret < 0) {
			fprintf(stderr, "Delegate error - msgrcv\n");
			break;
		}
		
		if((*dq).type == 1) {
			// żądanie prac na działce Teren[i]
			// Sprawdzenie czy warunki są spełnione
			dq->result = true;
			
			if(p.pocz <= (*dq).arg1 && (*dq).arg1 < p.pocz + p.pracownicy)
				dq->result = false;
			
			int gl = 0;
			pthread_mutex_lock(&mutex);
			while(gl <= p.glebokosc && stan[(*dq).arg1][gl] == 0)
				++gl;
			
			// Odczytanie symbolu
			if(dzialki[dq->arg1] == -1 && gl <= p.glebokosc 
			                           && stan[(*dq).arg1][gl] == 1) {
				(*dq).result = true;
				(*dq).arg1 = teren[(*dq).arg1][gl];
				dzialki[(*dq).arg1] = gl;
			} else {
				dq->result = false;
			}
			
			pthread_mutex_unlock(&mutex);
			
			msgsnd(mfq, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 0);
		} else if((*dq).type == 3) {
			// raport ze zbiorem znalezisk
			pthread_mutex_lock(&mutex);
			
			stan[(*dq).arg1][dzialki[(*dq).arg1]] = 0;
			teren[(*dq).arg1][dzialki[(*dq).arg1]] = 0;
			szacunek[(*dq).arg1][dzialki[(*dq).arg1]] = 0;
			
			--do_wydobycia;
			
			pthread_mutex_unlock(&mutex);
			
			dzialki[(*dq).arg1] = -1;
		} else if((*dq).type == 4) {
			// zwrot pozwolenia
			pthread_mutex_lock(&mutex);
			
			for(int i = p.pocz; i < p.pocz + p.pracownicy; ++i)
				for(int j = 0; j <= p.glebokosc; ++j)
					if(stan[i][j] == 1)
						stan[i][j] = 2;
			
			pthread_mutex_unlock(&mutex);
			
			free(dq);
			break;
		}
		
		free(dq);
	}
	
	return 0;
}

int main(int argc, char *argv[])
{
	if(argc <= 4) {
		printf("Zle argumenty uruchomienia\n");
		return 1;
	}

	// Sparsowanie parametrów
	sscanf(argv[1], "%d", &dlugosc);
	sscanf(argv[2], "%d", &glebokosc);
	sscanf(argv[3], "%d", &S);
	sscanf(argv[4], "%d", &A);

	przygotuj();

	// Działalność
	int mfq = msgget(MUZEUM_FIRMA_Q, 0660);
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	for(;;) {
		// Sprawdzenie czy wszystkie pola są wydobyte
		pthread_mutex_lock(&mutex);
		
		if(do_wydobycia == 0) {
			pthread_mutex_unlock(&mutex);
			break;
		}
		
		pthread_mutex_unlock(&mutex);
		
		// Otrzymanie prośby o pozwolenie
		struct pozwolenie pozw;
		int ret = msgrcv(mfq, &pozw, sizeof(struct pozwolenie), 1, 0);
		
		if(ret < 0) {
			fprintf(stderr, "msgrcv error - prosba o pozwolenie\n");
			fprintf(stderr, "ABORTING\n");
			break;
		}
		
		if(pozw.id_firmy == -1) 
			break;
		
		// Znalezienie działki
		pthread_mutex_lock(&mutex);
		pozw.czy_ok = szukaj_pozwolenia(pozw.pracownicy, pozw.oplata, 
		                                &pozw.pocz, &pozw.glebokosc);
		
		pozw.port = MUZEUM_FIRMA_OFFSET;
		pozw.port += 2 * pozw.id_firmy;
		
		// Pobranie opłaty
		struct bank_query bankq;
		bankq.type = 3;
		bankq.account = pozw.id_firmy;
		bankq.value = (pozw.czy_ok ? pozw.oplata : S);
		msgsnd(mbq, &bankq, sizeof(bankq), 0);
		
		// Wysłanie pozwolenia lub odmowy
		msgsnd(mfq, &pozw, sizeof(pozw), 0);
		
		if(!pozw.czy_ok) {
			pthread_mutex_unlock(&mutex);
			continue;
		}
		
		// Zarezerwowanie jej
		for(int i = pozw.pocz; i < pozw.pocz + pozw.pracownicy; ++i)
			for(int j = 0; j <= pozw.glebokosc; ++j)
				if(stan[i][j] != 0)
					stan[i][j] = 1;
		
		// Utworzenie delegata
		pthread_t th;
		pthread_attr_t pat;
		pthread_attr_init(&pat);
		pthread_attr_setdetachstate(&pat, PTHREAD_CREATE_DETACHED);
		
		struct pozwolenie *p = malloc(sizeof(struct pozwolenie));
		*p = pozw;
		ret = pthread_create(&th, &pat, delegat, (void *) p);
        pthread_attr_destroy(&pat);
        
        if(ret < 0) {
			fprintf(stderr, "pthread_create error - tworzenie delegata\n");
			fprintf(stderr, "ABORTING\n");
			pthread_mutex_unlock(&mutex);
			break;
		}
		
		pthread_mutex_unlock(&mutex);
	}
	
	// Koniec
	koniec_dzialalnosci(0);

	return 0;
}
