a.out: Makefile c-minux.tab.c c-minux.tab.h main.c
	cc -DYYDEBUG=1 -fsanitize=address -g3 array.c eprintf.c c-minux.tab.c main.c -o ./a.out

c-minux.tab.c c-minux.tab.h: Makefile c-minux.y main.h
	bison -d c-minux.y
