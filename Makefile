# CFLAGS=-g
CFLAGS ?= -O3

all : schemer.bytecode interpreter trace-interpreter assembler

interpreter : interpreter.o
	gcc ${CFLAGS} -o interpreter interpreter.o

interpreter.o : interpreter.c interpreter.h
	gcc ${CFLAGS} -c interpreter.c

trace-interpreter : trace-interpreter.o
	gcc ${CFLAGS} -o trace-interpreter -D__TRACE__ trace-interpreter.o

trace-interpreter.o : interpreter.c interpreter.h
	gcc -c ${CFLAGS} -o trace-interpreter.o -D__TRACE__ interpreter.c

assembler : assembler.yy.o
	gcc ${CFLAGS} -o assembler assembler.yy.o

schemer.bytecode : assembler schemer.asm
	./assembler <schemer.asm >$@

schemer.asm : interpreter lib.sch schemer.sch schemer-bootstrap.bytecode
	cat lib.sch schemer.sch | ./interpreter schemer-bootstrap.bytecode > $@

schemer-bootstrap.bytecode: assembler schemer-bootstrap.asm
	./assembler <schemer-bootstrap.asm > $@

schemer-bootstrap.asm : schemer.sch lib.sch
	cat lib.sch schemer.sch | guile --use-srfi=13 schemer.sch > $@

assembler.yy.o : assembler.yy.c
	gcc ${CFLAGS} -c assembler.yy.c

assembler.yy.c : assembler.l interpreter.h
	lex -o assembler.yy.c assembler.l

test: schemer-bootstrap.asm schemer.asm
	diff -q schemer-bootstrap.asm schemer.asm

clean : 
	rm -f *~ assembler.yy.c *.o *.asm *-bootstrap*

distclean : clean
	rm -f interpreter trace-interpreter assembler *.asm *.bytecode