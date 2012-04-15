# Copyright 2012 J. Aaron Pendergrass

# This file is part of toy-bytecode.

# toy-bytecode is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# toy-bytecode is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with toy-bytecode.  If not, see <http://www.gnu.org/licenses/>.

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

%.asm :  %.sch schemer.bytecode lib.sch interpreter
	cat lib.sch $< | ./interpreter schemer.bytecode > $@

%.bytecode : %.asm assembler
	./assembler < $< > $@

clean : 
	rm -f *~ assembler.yy.c *.o *.asm *-bootstrap* 

distclean : clean
	rm -f interpreter trace-interpreter assembler *.asm *.bytecode