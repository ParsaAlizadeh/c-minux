a.out: Makefile c-minux.tab.c c-minux.tab.h main.c codegen.c printer.c
	cc -DYYDEBUG=1 -fsanitize=address -g3 `llvm-config --cflags --ldflags --libs core support` array.c eprintf.c c-minux.tab.c codegen.c main.c -o ./a.out

c-minux.tab.c c-minux.tab.h: Makefile c-minux.y main.h
	bison -d c-minux.y

%.exe: %.s
	clang $< -o $@

%.s: %.optbc
	llc -relocation-model=pic $< -o $@

%.optbc: %.bc
	opt -O2 $< -o $@

%.bc: %.ll
	llvm-as $< -o $@

output.ll: a.out input.txt
	./a.out

clear:
	rm -f *.exe *.s *.optbc *.bc *.ll a.out
