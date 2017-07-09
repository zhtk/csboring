# ZSO - Zadanie 3

Patch dopisuje do syscalla `ptrace` operację `PTRACE_RUN_SYSCALL`,
wykonującą dany syscall jako target. Użyta wersja kernela: 4.9.13.

Schemat wykonania taki jak w treści zadania. Do wywołania syscalla dochodzi w
pętli obsługującej sygnały etc. przed skokiem w userspace, wykorzystuję
do tego funkcję do_syscall_64. Do przechwytywania wyniku wykorzystuję hooki
jak przy SYSCALL_TRACE. Potem zatrzymuję proces i następuje reschedule.
W przypadku śmierci śledzonego procesu wznawiane jest wykonanie śledzącego.

## Znane błędy

* Nie działa wykonywanie syscalla `fork` - następuje zawieszenie procesu.
* Nie pozwalanie na zabicie tracera (np. SIGKILL) podczas syscalla.

## Instalacja

Patcha można aplikować przy pomocy polecenia `patch -p1` na źródła kernela.
