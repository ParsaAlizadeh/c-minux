#ifndef EPRINTF_H
#define EPRINTF_H

#include <stdarg.h>
#include <stdlib.h>

/*
 * Name of the program, usually argv[0], is printed before fmt in the
 * eprintf.
 */
extern void setprogname(const char *);
extern const char *getprogname(void);

/*
 * Print to stderr. eprintf exits with code 2. if last character of
 * fmt is ':', strerror(errno) will also be printed. Always prints \n
 * after fmt
 */
extern void vweprintf(const char *fmt, va_list);
extern void weprintf(const char *fmt, ...);
extern void eprintf(const char *fmt, ...);

/*
 * On error, these functions run eprintf to stop the program.
 */
extern void *emalloc(size_t);
extern void *erealloc(void *, size_t);
extern char *estrdup(const char *);

#endif
