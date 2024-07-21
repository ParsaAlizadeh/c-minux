a.out: Makefile c-minux.tab.c c-minux.tab.h main.c codegen.c
	cc -DYYDEBUG=1 -fsanitize=address -g3 `llvm-config --cflags --ldflags --libs core support` array.c eprintf.c c-minux.tab.c codegen.c main.c -o ./a.out

c-minux.tab.c c-minux.tab.h: Makefile c-minux.y main.h
	bison -d c-minux.y
