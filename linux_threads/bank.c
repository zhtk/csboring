#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <pthread.h>

#include "config.h"

struct dane_firmy
{
	int Fid;
	int Fsaldo;
	pid_t proces_id;
	bool aktywna;
};

int F, S, A;
int aktywne_firmy = 0;
struct dane_firmy *firmy;
pthread_mutex_t mutex;

void uruchom_firme(int nr)
{
	++aktywne_firmy;
	
	int ile_pracownikow;
	scanf("%d%d%d", &firmy[nr].Fid, &firmy[nr].Fsaldo, &ile_pracownikow);
	
	char fid[15], fsaldo[15], fprac[15], ca[15], cs[15];
	sprintf(fid, "%d", firmy[nr].Fid);
	sprintf(fsaldo, "%d", firmy[nr].Fsaldo);
	sprintf(fprac, "%d", ile_pracownikow);
	sprintf(ca, "%d", A);
	sprintf(cs, "%d", S);
	
	switch(firmy[nr].proces_id = fork())
	{
	case -1:
		fprintf(stderr, "Bank: Error in fork\n");
		exit(1);
	case 0: 	
		execl("./firma", "firma", fid, fsaldo, fprac, ca, cs, (char *) 0);
		fprintf(stderr, "Bank -> Firma: Error in execl\n");
		exit(1);
	default:
		firmy[nr].aktywna = true;
		break;
	}
}

void* stan_konta(void *arg)
{
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	if(mbq < 0) {
		fprintf(stderr, "MUZEUM_BANK_Q - kolejka nie istnieje\n");
		exit(1);
	}
	
	for(;;) {
		struct bank_query query;
		msgrcv(mbq, &query, sizeof(struct bank_query), 1, 0);
		query.value = -1;
		
		pthread_mutex_lock(&mutex);
		for(int i = 0; i < F; ++i)
			if(firmy[i].Fid == query.account) {
				query.value = firmy[i].Fsaldo;
				break;
			}
		pthread_mutex_unlock(&mutex);
		
		query.type = 2;
		msgsnd(mbq, &query, sizeof(struct bank_query), 0);
	}
	
	return NULL;
}

void* przelew_z_konta(void *arg)
{
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	if(mbq < 0) {
		fprintf(stderr, "MUZEUM_BANK_Q - kolejka nie istnieje\n");
		exit(1);
	}
	
	for(;;) {
		struct bank_query query;
		msgrcv(mbq, &query, sizeof(struct bank_query), 3, 0);
		
		pthread_mutex_lock(&mutex);
		for(int i = 0; i < F; ++i)
			if(firmy[i].Fid == query.account) {
				firmy[i].Fsaldo -= query.value;
				break;
			}
		pthread_mutex_unlock(&mutex);
	}
	
	return NULL;
}

void* przelew_na_konta(void *arg)
{
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	if(mbq < 0) {
		fprintf(stderr, "MUZEUM_BANK_Q - kolejka nie istnieje\n");
		exit(1);
	}
	
	for(;;) {
		struct bank_query query;
		msgrcv(mbq, &query, sizeof(struct bank_query), 4, 0);
		
		pthread_mutex_lock(&mutex);
		for(int i = 0; i < F; ++i)
			if(firmy[i].Fid == query.account) {
				firmy[i].Fsaldo += query.value;
				break;
			}
		pthread_mutex_unlock(&mutex);
	}
	
	return NULL;
}

// Obsługuje zakończenie działalności firmy
void* firma_konczy(void *arg)
{
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	if(mbq < 0) {
		fprintf(stderr, "MUZEUM_BANK_Q - kolejka nie istnieje\n");
		exit(1);
	}
	
	for(;;) {
		struct bank_query query;
		msgrcv(mbq, &query, sizeof(struct bank_query), 7, 0);
		
		pthread_mutex_lock(&mutex);
		for(int i = 0; i < F; ++i)
			if(firmy[i].Fid == query.account) {
				firmy[i].aktywna = false;
				--aktywne_firmy;
				break;
			}
		pthread_mutex_unlock(&mutex);
		
		if(aktywne_firmy == 0) {
			// Zakończenie pracy muzeum
			struct pozwolenie p;
			p.port = 1;
			p.id_firmy = -1;
			int mfq = msgget(MUZEUM_FIRMA_Q, 0660);
			msgsnd(mfq, &p, sizeof(struct pozwolenie), 0);
			
			// Zakończenie pracy banku
			query.type = 5;
			msgsnd(mbq, &query, sizeof(struct bank_query), 0);
		}
	}
	
	return NULL;
}

// Koniec działalności banku
void koniec_dzialalnosci(int s)
{
	pthread_mutex_lock(&mutex);
	
	// Żądanie zakończenia prac w firmach
	for(int i = 0; i < F; ++i)
		if(firmy[i].aktywna)
			kill(firmy[i].proces_id, SIGINT);
	
	// Zaczekanie aż firmy skończą pracę
	int st;
	
	for(int i = 0; i < F; ++i)
		if(firmy[i].aktywna)
			waitpid(firmy[i].proces_id, &st, 0);
	
	// Zwolnienie zasobów
	free(firmy);
	pthread_mutex_destroy(&mutex);
	
	// Koniec procesu
	exit(0);
}

// Przygotowuje działalność banku
void przygotuj()
{
	pthread_mutex_init(&mutex, NULL);
	firmy = calloc(F, sizeof(struct dane_firmy));
	
	// Wysyła do muzeum ilość firm
	int mfq = msgget(MUZEUM_FIRMA_Q, 0660);
	struct delegat_query dq;
	dq.port = 5;
	dq.arg1 = F;
	msgsnd(mfq, &dq, sizeof(struct delegat_query), 0);
	
	// Wczytuje dane
	for(int i = 0; i < F; ++i)
		uruchom_firme(i);
	
	// Tworzy wątki do obsługi zapytań
	pthread_t th1, th2, th3, th4;
	pthread_attr_t pat;
	pthread_attr_init(&pat);
	pthread_attr_setdetachstate(&pat, PTHREAD_CREATE_DETACHED);
	int ret = pthread_create(&th1, &pat, stan_konta, NULL);
	ret |= pthread_create(&th2, &pat, przelew_na_konta, NULL);
	ret |= pthread_create(&th3, &pat, przelew_z_konta, NULL);
	ret |= pthread_create(&th4, &pat, firma_konczy, NULL);
	
	if(ret < 0) {
		fprintf(stderr, "pthread - nie udało się utworzyć wątku\n");
		exit(1);
	}
	
	pthread_attr_destroy(&pat);
	
	// Ustawia obsługę SIGINTa
	struct sigaction sa;
	sa.sa_handler = koniec_dzialalnosci;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_RESETHAND;
	sigaction(SIGINT, &sa, NULL);
}

int main(int argc, char *argv[])
{
	if(argc <= 3) {
		printf("Zle argumenty uruchomienia\n");
		return 1;
	}

	// Sparsowanie parametrów
	sscanf(argv[1], "%d", &F);
	sscanf(argv[2], "%d", &S);
	sscanf(argv[3], "%d", &A);

	przygotuj();

	// Oczekiwanie na koniec
	int mbq = msgget(MUZEUM_BANK_Q, 0660);
	
	if(mbq < 0) {
		fprintf(stderr, "MUZEUM_BANK_Q - kolejka nie istnieje\n");
		exit(1);
	}
	
	struct bank_query query;
	msgrcv(mbq, &query, sizeof(struct bank_query), 5, 0);
	
	// Koniec
	koniec_dzialalnosci(0);

	return 0;
}
