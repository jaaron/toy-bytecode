
all : interpreter trace-interpreter assembler

interpreter : interpreter.o
	gcc -O3 -o interpreter interpreter.o

interpreter.o : interpreter.c interpreter.h
	gcc -O3 -c interpreter.c

trace-interpreter : trace-interpreter.o
	gcc -O3 -o trace-interpreter -D__TRACE__ trace-interpreter.o

trace-interpreter.o : interpreter.c interpreter.h
	gcc -c -O3 -o trace-interpreter.o -D__TRACE__ interpreter.c

assembler : assembler.yy.o
	gcc -O3 -o assembler assembler.yy.o

assembler.yy.o : assembler.yy.c
	gcc -O3 -c assembler.yy.c

assembler.yy.c : assembler.l interpreter.h
	lex -o assembler.yy.c assembler.l

clean : 
	rm -f *~ assembler.yy.c *.o

distclean : clean
	rm -f interpreter trace-interpreter assembler