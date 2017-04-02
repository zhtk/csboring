#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <pthread.h>

#include "config.h"

// Kolejka do komunikacji z bankiem
int bank_msg;
// Kolejka do komunikacji z muzeum
int muzeum_msg;
// Ewidencja posiadanych artefaktów
int *artefakty;
// Dane firmy, Fid, ilość pieniędzy na koncie
int Fid, Saldo = 0;
// Ilość pracowników oraz wolnych pracowników
int pracownicy, ile_wolnych = 0;
// Stałe
int A, S;
// Mutex
pthread_mutex_t mutex;

// Zmienne służące do przydzielania pracy pracownikowi
int dig_it, dzialka;
pthread_cond_t czekam, szukam_pracy;

// Zmienna do wstrzymywania pracy firmy
bool is_stopped = false;
pthread_cond_t blocker;

// Raport końcowy firmy, wysyła go do muzeum
// Zakłada że dostęp do danych jest zablokowany
void wypisz_stan()
{
	struct delegat_query *dq;
	dq = malloc(sizeof(struct delegat_query) + sizeof(int)*(A+1));
	dq->port = 3;
	dq->type = Fid;
	dq->arg1 = Saldo;
	
	for(int i = 0; i <= A; ++i)
		dq->arg2[i] = artefakty[i];
		
	msgsnd(muzeum_msg, &dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 0);
	free(dq);
}

// Wątek pracownika
void* pracownik_thread(void *arg)
{
	for(;;) {
		// Pobranie pracy do wykonania
		pthread_mutex_lock(&mutex);
		
		if(dig_it > 0)
			pthread_cond_signal(&szukam_pracy);
		
		++ile_wolnych;
		pthread_cond_wait(&czekam, &mutex);
		
		int liczba = dig_it;
		int kwatera = dzialka;
		--ile_wolnych;
		dig_it = 0;
		
		pthread_mutex_unlock(&mutex);
		
		// Kopanie
		int znaleziska[A+1];
		for(int i = 0; i <= A; ++i) {
			znaleziska[i] = 0;
			
			if(i < 2)
				continue;
			
			while(liczba % i == 0)
				++znaleziska[i], liczba /= i;
		}
		
		// Wysłanie raportu
		struct delegat_query *dq;
		dq = malloc(sizeof(struct delegat_query) + sizeof(int)*(A+1));
		dq->port = MUZEUM_FIRMA_OFFSET + 2 * Fid + 1;
		dq->type = 3;
		dq->arg1 = kwatera;
		
		for(int i = 0; i <= A; ++i)
			dq->arg2[i] = znaleziska[i];
		
		msgsnd(muzeum_msg, &dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 0);
		free(dq);
		
		// Przeniesienie artefaktów do magazynu
		pthread_mutex_lock(&mutex);
		
		for(int i = 0; i <= A; ++i)
			artefakty[i] += znaleziska[i];
		
		// Zawieszenie działalności (jeśli trzeba)
		while(is_stopped)
			pthread_cond_wait(&blocker, &mutex);
		pthread_cond_signal(&blocker);
		
		pthread_mutex_unlock(&mutex);
	}
	
	return NULL;
}

// Koniec totalny działalności firmy
void koniec_dzialalnosci(int s)
{
	pthread_mutex_lock(&mutex);
	
	// Zwrot pozwolenia
	struct delegat_query dq;
	dq.port = MUZEUM_FIRMA_OFFSET + 2 * Fid + 1;
	dq.type = 4;
	msgsnd(muzeum_msg, &dq, sizeof(struct delegat_query), 0);
	
	// Wysłanie do banku zawiadomienia o zakończeniu pracy
	struct bank_query bq;
	bq.type = 7;
	bq.account = Fid;
	msgsnd(bank_msg, &bq, sizeof(struct bank_query), 0);
	
	// Raport kończący
	wypisz_stan();

	// Zwolnienie pamięci
	free(artefakty);
	
	pthread_mutex_destroy(&mutex);
	pthread_cond_destroy(&blocker);
	pthread_cond_destroy(&czekam);

	// Koniec procesu
	exit(0);
}

void wstrzymaj(int s)
{
	if(s == SIGUSR1) {
		is_stopped = true;
	} else if(s == SIGUSR2) {
		is_stopped = false;
		pthread_cond_signal(&blocker);
	}
}

// Przygotowuje działalność firmy
void przygotuj()
{
	pthread_cond_init(&blocker, NULL);
	pthread_cond_init(&czekam, NULL);
	pthread_mutex_init(&mutex, NULL);
	
	// Kolejka dla banku
	if ((bank_msg = msgget(MUZEUM_BANK_Q, 0660)) == -1) {
		fprintf(stderr, "Firma - MUZEUM_BANK_Q - retrieval error\n");
		exit(1);
	}
	
	// Kolejka dla muzeum
	if ((muzeum_msg = msgget(MUZEUM_FIRMA_Q, 0660)) == -1) {
		fprintf(stderr, "Firma - MUZEUM_BANK_Q - retrieval error\n");
		exit(1);
	}
	
	// Utworzenie bufora na kolekcje
	artefakty = calloc(A+1, sizeof(int));
	
	for(int i = 0; i <= A; ++i)
		artefakty[i] = 0;
	
	// Ustawia obsługę SIGINTa
	struct sigaction sa;
	sa.sa_handler = koniec_dzialalnosci;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_RESETHAND;
	sigaction(SIGINT, &sa, NULL);
	
	// Ustawia obsługę SIGUSR
	sa.sa_handler = wstrzymaj;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_RESETHAND;
	sigaction(SIGUSR1, &sa, NULL);
	sigaction(SIGUSR2, &sa, NULL);
	
	// Tworzy wątki pracowników
	pthread_t th;
	pthread_attr_t pat;
	pthread_attr_init(&pat);
	pthread_attr_setdetachstate(&pat, PTHREAD_CREATE_DETACHED);
	
	for(int i = 0; i < pracownicy; ++i)
		if(pthread_create(&th, &pat, pracownik_thread, NULL)) {
			fprintf(stderr, "Firma - nie mozna utworzyc pracownikow\n");
			exit(1);
		}
	
	pthread_attr_destroy(&pat);
}

// Rozdzielanie pracy między wątki
void rozdzielaj_prace(struct pozwolenie p)
{
	bool cos_zostalo = true;
	
	while(cos_zostalo) {
		cos_zostalo = false;
		
		struct delegat_query *dq;
		dq = malloc(sizeof(struct delegat_query) + sizeof(int)*(A+1));
		
		for(int i = 0; i < pracownicy; ++i) {
			dq->port = MUZEUM_FIRMA_OFFSET + 2 * Fid + 1;
			dq->type = 1;
			dq->arg1 = p.pocz + i;
			
			msgsnd(muzeum_msg, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), 0);
			msgrcv(muzeum_msg, dq, sizeof(struct delegat_query) + sizeof(int)*(A+1), dq->port - 1, 0);
			
			if(dq->result == false)
				continue;
			
			pthread_mutex_lock(&mutex);
			
			dig_it = dq->arg1;
			dzialka = p.pocz + i;
		
			// jeśli nie ma wolnych wątków to powieś się na "szukam_pracy"
			if(ile_wolnych == 0)
				pthread_cond_wait(&szukam_pracy, &mutex);
			pthread_cond_signal(&czekam);
			
			pthread_mutex_unlock(&mutex);
		}
		
		free(dq);
	}
}

int main(int argc, char *argv[])
{
	sscanf(argv[1], "%d", &Fid);
	sscanf(argv[2], "%d", &Saldo);
	sscanf(argv[3], "%d", &pracownicy);
	sscanf(argv[4], "%d", &A);
	sscanf(argv[5], "%d", &S);
	
	przygotuj();
		                          
	// Działalność i podział pracy
	for(;;) {
		// Sprzedaż posiadanych artefaktów
		pthread_mutex_lock(&mutex);
		for(int i = 0; i <= A; ++i)
			while(artefakty[i] >= i) {
				struct delegat_query dq;
				dq.port = 2;
				dq.type = Fid;
				dq.arg1 = i;
				msgsnd(muzeum_msg, &dq, sizeof(struct delegat_query), 0);
				
				Saldo += 10 * i;
			}
		pthread_mutex_unlock(&mutex);
		
		// Koniec działaności
		if(Saldo < S)
			break;
		
		// Prośba o pozwolenie
		pthread_mutex_lock(&mutex);
		
		struct pozwolenie p;
		p.port = 1;
		p.id_firmy = Fid;
		p.pracownicy = pracownicy;
		p.oplata = Saldo;
		
		pthread_mutex_unlock(&mutex);
		
		if(msgsnd(muzeum_msg, &p, sizeof(struct pozwolenie), 0) < 0) {
			fprintf(stderr, "Firma - problem z ubieganiem sie o pozwolenie\n");
			exit(1);
		}
		
		if(msgrcv(muzeum_msg, &p, sizeof(struct pozwolenie), 
		          MUZEUM_FIRMA_OFFSET + 2 * Fid, 0) < 0) {
			fprintf(stderr, "Firma - problem z ubieganiem sie o pozwolenie\n");
			exit(1);
		}
		
		if(!p.czy_ok)
			continue;
		
		rozdzielaj_prace(p);
	}
	
	// Koniec
	koniec_dzialalnosci(0);

	return 0;
}
