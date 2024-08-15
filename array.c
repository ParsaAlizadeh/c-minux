#include <stdio.h>
#include <stdarg.h>
#include "array.h"

int Sprintf(Str * restrict s, const char * restrict fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    size_t len = StrLen(s);
    int nbyte = vsnprintf(&s->arr[len], s->cap - len, fmt, ap);
    va_end(ap);
    if (nbyte < 0)
        return -1;
    size_t newlen = len + nbyte + 1;
    if (newlen > s->cap) {
        ArrayPinchN(s, newlen - s->len);
        va_start(ap, fmt);
        if (vsnprintf(&s->arr[len], nbyte + 1, fmt, ap) != nbyte) {
            va_end(ap);
            return -1;
        }
        va_end(ap);
    }
    s->len = newlen;
    return nbyte;
}
