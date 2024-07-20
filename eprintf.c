#include "eprintf.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static const char *progname = NULL;

void setprogname(const char *s) {
    progname = s;
}

const char *getprogname(void) {
    return progname;
}

void vweprintf(const char *fmt, va_list ap) {
    fflush(stdout);
    if (getprogname() != NULL)
        fprintf(stderr, "%s: ", getprogname());
    vfprintf(stderr, fmt, ap);
    if (fmt[strlen(fmt)-1] == ':')
        fprintf(stderr, " %s", strerror(errno));
    fprintf(stderr, "\n");
}

void weprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vweprintf(fmt, ap);
    va_end(ap);
}

void eprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vweprintf(fmt, ap);
    va_end(ap);
    exit(2);
}

void *emalloc(size_t size) {
    void *p = malloc(size);
    if (p == NULL)
        eprintf("emalloc(%lu) failed:", size);
    return p;
}

void *erealloc(void *p, size_t size) {
    void *newp = realloc(p, size);
    if (newp == NULL)
        eprintf("erealloc(%p, %lu) failed:", p, size);
    return newp;
}

char *estrdup(const char *s) {
    char *news = strdup(s);
    if (news == NULL)
        eprintf("estrdup(\"%.50s\") failed:", s);
    return news;
}
