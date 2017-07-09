#include "interceptor.h"

#define _GNU_SOURCE
#include <elf.h>
#include <link.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

// Wskaźnik na oryginalną funkcję, która jest w danej chwili zastępowana
static void *old_function = NULL;
// Wskaźnik na nową funkcję
static void *new_function = NULL;
// Wskaźnik na nazwę podmienianej funkcji
static const char *fun_name = NULL;
// Wskaźnik na listę podmienionych funkcji
static struct funinfo *intfun = NULL;

struct dyninfo {
	// Adres bazowy segmentu
	ElfW(Addr) addr;
	
	// Parametry tablicy symboli
	ElfW(Addr) symtab;
	ElfW(Sword) syment;
	
	// Parametry tablicy napisów
	ElfW(Addr) strtab;
	
	// Tablice relokacji
	// DT_PLTREL = DT_REL lub DT_RELA - rodzaj relokacji
	ElfW(Sword) pltrel;
	ElfW(Sword) pltrelsz;
	// Wskaźnik na spis treści GOT
	ElfW(Addr) jmprel;
};

struct funinfo {
	void *fun;
	const char *name;
	struct funinfo *next;
};

static void *fun_lookup(const char *name)
{
	struct funinfo *list = intfun;
	
	while (list != NULL) {
		if (!strcmp(name, list->name))
			return list->fun;
		
		list = list->next;
	}
	
	return NULL;
}

static void fun_insert(const char *name, void *fun)
{
	if (fun == NULL)
		return;
	
	struct funinfo **list = &intfun;
	
	while (*list != NULL) {
		if (!strcmp(name, (**list).name))
			return;
		
		*list = (**list).next;
	}
	
	*list = malloc(sizeof(struct funinfo));
	(**list).next = NULL;
	(**list).name = name;
	(**list).fun = fun;
}

static void load_dynamic_info(ElfW(Dyn) *addr, struct dyninfo *dyn)
{
	// Przejście po wszystkich wpisach w segmencie i zlokalizowanie
	// potrzebnych struktur
	for (int j = 0; addr[j].d_tag != DT_NULL; ++j) {
		// Tablica symboli
		if (addr[j].d_tag == DT_SYMTAB)
			dyn->symtab = addr[j].d_un.d_ptr;
		
		if (addr[j].d_tag == DT_SYMENT)
			dyn->syment = addr[j].d_un.d_val;
		
		// Tablica stringów
		if (addr[j].d_tag == DT_STRTAB)
			dyn->strtab = addr[j].d_un.d_ptr;
				
		// Tablica relokacji
		if (addr[j].d_tag == DT_PLTREL)
			dyn->pltrel = addr[j].d_un.d_val;
		
		if (addr[j].d_tag == DT_PLTRELSZ)
			dyn->pltrelsz = addr[j].d_un.d_val;
		
		if (addr[j].d_tag == DT_JMPREL)
			dyn->jmprel = addr[j].d_un.d_ptr;
	}
}

static void force_load(struct dyninfo *dyn, ElfW(Addr) *entry)
{
	// Sprawdzenie, czy adres funkcji jest w segmencie
	// Wywołanie dynamicznego linkera
	// TODO
}

static void alter_got(struct dyninfo *dyn, ElfW(Addr) offset)
{
	// Adres wpisu GOT podmienianej funkcji
	ElfW(Addr) *entry = (ElfW(Addr) *) (dyn->addr + offset);
	
	// Jeżeli adres starej funkcji wskazuje na dynamiczny linker to
	// trzeba go rozwiązać, inaczej po pierwszym jej wywołaniu adres w GOT
	// zostanie podmieniony
	force_load(dyn, entry);
	
	// Zapisanie starej funkcji
	if (old_function == NULL)
		old_function = (void *) *entry;
	
	// Podmiana na nową funkcję
	*entry = (ElfW(Addr)) new_function;
}

static void alter_table(char *base, int esize, int tsize, struct dyninfo *dyn)
{
	// Ilość rekordów relokacji
	int relsize = tsize / esize;
	
	// Tablica symboli
	ElfW(Sym) *symtab = (ElfW(Sym) *) dyn->symtab;
	
	// Tablica napisów
	char *strtab = (char *) dyn->strtab;
	
	for (int i = 0; i < relsize; ++i) {
		ElfW(Rel) *entry = (ElfW(Rel) *) (base + i * esize);
		
		// Interesują nas tylko relokacje funkcji w GOT
		if (ELF64_R_TYPE(entry->r_info) != R_X86_64_JUMP_SLOT)
			continue;
		
		// Indeks obiektu w tablicy symboli
		int symindex = ELF64_R_SYM(entry->r_info);
		
		// Pomijamy niezdefiniowane symbole
		if (symindex == 0)
			continue;
		
		// Pomijamy symbole niebędące funkcjami
		if (ELF64_ST_TYPE(symtab[symindex].st_info) != STT_FUNC)
			continue;
		
		// Indeks w tablicy nazw symboli
		int strind = symtab[symindex].st_name;
		
		// Sprawdzenie, czy nazwa przechwytywanej funkcji pasuje do wpisu
		if (!strind || strcmp(fun_name, strtab + strind))
			continue;
		
		// Podmiana wpisu w GOT
		alter_got(dyn, entry->r_offset);
	}
}

static void alter_segment(ElfW(Dyn) *addr, struct dyninfo *dyn)
{
	// Skanuje relokacje "R_X86_64_JUMP_SLOT" powiązane z PLT
	// Format wpisów zależy od DT_PLTREL
	if (dyn->pltrel == DT_RELA) {
		// Tylko x86_64 ?
		alter_table((char *) dyn->jmprel, sizeof(ElfW(Rela)), dyn->pltrelsz, dyn);
	} else if (dyn->pltrel == DT_REL) {
		// Tylko x86 ?
		alter_table((char *) dyn->jmprel, sizeof(ElfW(Rel)), dyn->pltrelsz, dyn);
	} else {
		// Nie jest ustawiony, pomijamy
	}
}

static int iterator(struct dl_phdr_info *info, size_t size, void *data)
{
	// Iteracja po kolejnych nagłówkach segmentów ("Program Header")
	for (int i = 0; i < info->dlpi_phnum; ++i) {
		// Sprawdzenie, czy nagłówek ma typ PT_DYNAMIC
		if (info->dlpi_phdr[i].p_type != PT_DYNAMIC)
			continue;
		
		// Obliczenie adresu struktury w pamięci programu
		ElfW(Dyn) *addr = (ElfW(Dyn) *) (info->dlpi_addr + info->dlpi_phdr[i].p_vaddr);
		
		struct dyninfo dyn;
		memset(&dyn, 0, sizeof(struct dyninfo));
		dyn.addr = info->dlpi_addr;
		
		// Załadowanie informacji o segmencie
		load_dynamic_info(addr, &dyn);
		
		// Zmiana danych w segmencie
		alter_segment(addr, &dyn);
	}
	
	return 0;
}

void *intercept_function(const char *name, void *new_func) 
{
	// Sprawdzenie, czy funkcja była kiedyś nadpisana
	old_function = fun_lookup(name);
	
	// Przejście po nagłówkach bibliotek
	fun_name = name;
	new_function = new_func;
	dl_iterate_phdr(iterator, NULL);
	
	// Zapisanie starej funkcji na liście
	fun_insert(name, old_function);
	
	// Zwrócenie wskaźnika na starą funkcję
	return old_function;
}

void unintercept_function(const char *name) 
{
	// Pobranie oryginalnej funkcji z listy
	old_function = fun_lookup(name);
	
	// Przywrócenie jej
	if (old_function != NULL)
		intercept_function(name, old_function);
}
