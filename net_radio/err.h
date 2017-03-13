#ifndef _ERR_
#define _ERR_

/* Wypisuje informację o błędnym zakończeniu funkcji systemowej
i kończy działanie programu. */
extern void syserr(const char *fmt, ...);

/* Wypisuje informację o błędzie i kończy działanie programu. */
extern void fatal(const char *fmt, ...);

/* Wypisuje informację o błędzie i kończy działanie programu. 
Pozwala ustwić kod błędu. */
extern void extended_fatal(int code, const char *fmt, ...);

#endif
