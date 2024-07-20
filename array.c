#include <stdio.h>
#include <stdarg.h>
#include "array.h"

int Sprintf(Str *s, const char *fmt, ...) {
    va_list ap, aq;
    va_start(ap, fmt);
    va_copy(aq, ap);
    int nbyte = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (nbyte < 0) {
        va_end(aq);
        return nbyte;
    }
    size_t newlen = StrLen(s) + nbyte + 1;
    ArrayPinchN(s, newlen - s->len);
    vsnprintf(&s->arr[StrLen(s)], nbyte+1, fmt, aq);
    s->len = newlen;
    va_end(aq);
    return nbyte;
}
