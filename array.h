#ifndef ARRAY_H
#define ARRAY_H

#include <stdlib.h>
#include <string.h>
#include "eprintf.h"

#define Array(T) struct { size_t len, cap; T *arr; }

#define ArrayZero(A) memset((A), 0, sizeof(*(A)))

#define ArrayPinch(A) do {                                              \
        if ((A)->len == (A)->cap) {                                     \
            size_t newcap = (A)->cap == 0 ? 4 : (2 * (A)->cap);         \
            (A)->arr = erealloc((A)->arr, sizeof((A)->arr[0]) * newcap); \
            (A)->cap = newcap;                                          \
        }                                                               \
    } while (0)

#define ArrayPinchN(A, N) do {                                          \
        if ((A)->len + (N) > (A)->cap) {                                \
            size_t newcap = (A)->cap == 0 ? 4 : (A)->cap;               \
            while ((A)->len + (N) > newcap)                             \
                newcap *= 2;                                            \
            (A)->arr = erealloc((A)->arr, sizeof((A)->arr[0]) * newcap); \
            (A)->cap = newcap;                                          \
        }                                                               \
    } while (0)

#define Append(A, X) do {                       \
        ArrayPinch(A);                          \
        (A)->arr[(A)->len++] = (X);             \
    } while (0)

#define ArrayRelease(A) do {                    \
        free((A)->arr);                         \
        ArrayZero(A);                           \
    } while (0)

#define ArrayLast(A) ((A)->arr[(A)->len-1])

typedef Array(char) Str;

#define StrLen(A) ((A)->len == 0 ? 0 : ((A)->len - 1))

extern int Sprintf(Str * restrict s, const char * restrict fmt, ...);

#endif
