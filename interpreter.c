/* Copyright 2012 J. Aaron Pendergrass					*/
/*									*/
/* This file is part of Foobar.						*/
/*									*/
/* Foobar is free software: you can redistribute it and/or modify	*/
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation, either version 3 of the License, or	*/
/* (at your option) any later version.					*/
/*									*/
/* Foobar is distributed in the hope that it will be useful,		*/
/* but WITHOUT ANY WARRANTY; without even the implied warranty of	*/
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	*/
/* GNU General Public License for more details.				*/
/*									*/
/* You should have received a copy of the GNU General Public License	*/
/* along with Foobar.  If not, see <http://www.gnu.org/licenses/>.	*/


/*
  Bytecode virtual machines are a popular topic these days.  The
  popularity of high level dynamic languages makes improvements in all
  things bytecode big news. Some of my favorite projects to track are

       PyPy http://codespeak.net/pypy/
       Tamarin Tracing https://wiki.mozilla.org/Tamarin:Tracing
       and SquirrelFish http://trac.webkit.org/wiki/SquirrelFish

   I think the coolest area of research right now is in JIT (just in
   time) compilers.  But first, let's write an interpreter.
*/

/* First we include a number of standard C headers.  Nothing
   exciting */
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

/* 
   The file interpreter.h defines all the opcodes, and the basic types
   used by the interpreter.  The types are: integers, pointers, vm
   constants, and language constants.  

   All types used by the VM fit in a 32 bit word the top two bits are
   reserved for flags used to identify the type of the word.

   Pointers are packed to include a 18 bit size field, and a 12
   bit location pointer (the total memory size is also defined in
   interpreter.h to be 2^18 = 256k).

   VM constants include instructions, characters, and the boolean
   values true and false.  

   Languages are free to define up to 2^30 new constants that will be 
   treated as opaque blobs by the machine (only an equality operation is
   defined).
*/

#include "interpreter.h"

/* 
   First we'll define some macros for checking and unpacking the
   different VM types.  The corresponding packers are defined in the
   interpreter.h header file.
*/
#define PTR_TARGET(x)        (((uword)x) & PTR_TARGET_MASK)
#define PTR_SIZE(x)          ((((uword)x) >> PTR_TARGET_BITS) & PTR_SIZE_MASK)
#define NUM_TO_NATIVE(x)     ((typeof(x))((((int)x) << FLAG_BITS) >> FLAG_BITS))
#define CHAR_TO_NATIVE(x)    ((char)((x) & 0xff))
#define CELL_TYPE(x)         (((uword)x) & FLAG_MASK)
#define IS_NUM(x)            (CELL_TYPE(x) == NUM)
#define IS_PTR(x)            (CELL_TYPE(x) == PTR)
#define IS_LCONST(x)         (CELL_TYPE(x) == LCONST)
#define IS_VCONST(x)         (CELL_TYPE(x) == VCONST)
#define IS_CONST(x)          (IS_LCONST(x)  || IS_VCONST(x))
#define IS_BOOL(x)           (x == TRUE_VAL || x == FALSE_VAL)
#define IS_CHAR(x)           (IS_VCONST(x) && ((x) & CHAR_FLAG) == CHAR_FLAG)
#define IS_INS(x)            (IS_VCONST(x) && ( (x) < NR_INS))

/* 
   Our interpreter will be a basic stack machine.  Memory consists of
   a large array with the stack at the top growing down, the static
   code is at the bottom, then two heaps (one live, and one dead used
   for garbage collection).  The heap grows monotonically upward until
   it is about to collide either with the other heap, or the reserved
   stack space (depending on which heap is live) at which point the
   garbage collector is invoked.
*/
static word memory[MEM_SIZE];          /* all of memory */

static word *sp = &memory[MEM_SIZE-1]; /* The top 256 words of memory
					     are reserved for the stack */

static word *prog_end;                 /* pointer to the end of the
					     loaded program text */

static word *pc = memory;              /* the program counter */

static word heap_size;                 /* maximum size of a heap
					     (computed based on
					     prog_end) */

static word *upper_heap;               /* upper_heap == prog_end + heap_size; */

static word *heap_base;                /* pointer to the first cell of
					     the heap, Should always be
					     either prog_end, or
					     upper_head. */
				       
static word *hp;                       /* the heap pointer. */

static word  rr = MAKE_NUM(0);         /* the root regiseter can be
					     used by the language to
					     store an additional root for
					     gc (anything on the stack is
					     also treated as a root).
					     The intended use is as a
					     pointer to a structure
					     containing local variables. */

static word callstack[256];
static word* callstack_top = callstack;
static int single_step = 0;

/* 
   Access the element at offset x from the top of the stack.  (0 is
   top).
*/
#define STACK(x) *(sp+1+(x))

/* 
   Popping an element from the stack.  Note that since changes the
   stack pointer, it is not safe to call this macro in an expression
   that accesses the current sp.
*/
#define STACK_POP() *( ++sp )
#define ignore(x) ({(void)x;})
/* 
   Pushing an element on the stack.  Like popping this changes the
   stack pointer so use with care.
*/
#define STACK_PUSH(x) do{					\
	word __tmp = x;					\
	if(sp == &memory[MEM_SIZE - STACK_SIZE]){		\
	    fprintf(stderr, "Stack overflow imminent!\n");	\
	    DO_DUMP(stderr);					\
	    exit(-1);						\
	}							\
	*sp = __tmp;						\
	sp--;							\
    }while(0)

/* The current height of the stack. */
#define STACK_HEIGHT() ((memory + MEM_SIZE-1) - sp)

/*
  Function for generically printing the contents of a cell. 
  Used by the DO_DUMP macro below, and the inspector 
*/
static inline void print_cell(FILE *stream, word c){
    switch(CELL_TYPE(c)){
      case NUM:
	  fprintf(stream, "%"PRId32" (%"PRIx32")",
		  NUM_TO_NATIVE(c), c);
	break;
      case VCONST:
	if(IS_CHAR(c)){
	  fprintf(stream,"char: %c", CHAR_TO_NATIVE(c));
	}else if(IS_BOOL(c)){
	  fprintf(stream, "bool: %c",
		  c == TRUE_VAL ? 'T' : 'F');
	}else{
	    fprintf(stream, "vc: %0"PRIx32, c);
	}
	break;
      case LCONST:
	  fprintf(stream, "lc: %"PRId32,
		  NUM_TO_NATIVE(c));
	break;
      case PTR:
	fprintf(stream, "ptr: %d sz: %d",
		PTR_TARGET(c), PTR_SIZE(c));
	break;
      default:
	  fprintf(stream, "(%1x)?: %0"PRIx32,
		  CELL_TYPE(c), c & (~(0x3 << 30)));
      }
}

/* 
   The following macro is used to dump the current machine state (pc,
   heap pointer, stack pointer, and stack contents) to an output
   stream.  It's mostly intended for debugging purposes and bailing
   out on type errors.
*/
#define DO_DUMP(stream) do{						\
    int q=0;								\
    fprintf(stream, "pc: %ld, hp: %ld sp: %ld height: %ld\nstack:\n",	\
	    pc-memory, hp-memory, sp-memory, STACK_HEIGHT());		\
    while(q < STACK_HEIGHT() ){						\
	fprintf(stderr, "\t");						\
	print_cell(stderr, STACK(q));					\
	fprintf(stderr, "\n");						\
	q++;								\
    }									\
    fprintf(stream, "callstack:\n");					\
    for(q = 0;&callstack[q] < callstack_top;q++){			\
	fprintf(stderr, "\t%d\n", callstack[q]);			\
    }									\
  }while(0)

/* 
   This is where life starts to really get interesting.  The
   interpreter will use what's called an indirect threaded dispatching
   model.  The idea is that each opcode acts as an index into an
   array, the value in the array is the address of the native code
   implementing that instruction.  For a great discussion of different
   instruction dispatching techniques check out David Mandelin's blog
   post on SquirrelFish:
   http://blog.mozilla.com/dmandelin/2008/06/03/squirrelfish/

   Conveniently, the flag value used to specify a VM constant is a 0
   in the top two bits of the word, this means that converting between
   opcodes and their index in the array is a nop.
*/
#define IDX_FROM_INS(i) (i)


#ifndef __TRACE__
#ifndef __CHECK_INS__
/* 
   The cool thing about indirect threaded dispatching is that
   interpreting the next instruction is just a matter of jumping to
   the addres specified in the array of opcode handlers.  This is in
   contrast to a traditional interpreter model that looks more like a
   case statement embedded in a big while loop.  The indirect threaded
   approach requires no comparisons to figure out which handler to
   execute.

   The NEXT macro encapsulates this dispatching paradigm.  Every
   instruction handler we write will end with a call to NEXT.
*/ 
#define NEXT goto *instructions[IDX_FROM_INS(*pc++)]
#else
/* 
   This version of NEXT checks to ensure that the target of the PC is
   in fact an instruction before trying to execute it.  Without this
   check, the VM will start executing arbitrary machine code if given
   a bad instruction stream.  But the check adds about a 20%
   performance penalty.
*/
#define NEXT do{						\
	ASSERT_INS(*pc);					\
	goto *instructions[IDX_FROM_INS(*pc++)];		\
    }while(0)
#endif

/* 
   As implied above, instruction handlers have to have a specific
   format for everything to work nicely. We're going to use gcc's
   computed goto feature to build our array, so each instruction will
   start with a label naming the instruction, then some C code
   implementing the handler, then a call to NEXT.
*/
#define INSTRUCTION(n,x)			\
  n:						\
  if(single_step) inspector();			\
  x;						\
  NEXT

#else

/* 
    For debugging purpose it's convenient to be able to spit out a
    trace of every instruction as it is executed.  To do this, we just
    define NEXT and INSTRUCTION to print the trace info.  This is
    built using the trace-compiler target in the Makefile.
*/
#define NEXT do{							\
	ASSERT_INS(*pc);						\
	fprintf(stderr,"==================================\n");		\
	fprintf(stderr,"initial pc: %ld instruction: %"PRId32" ",	\
		pc-memory, IDX_FROM_INS(*pc));				\
	goto *instructions[IDX_FROM_INS(*pc++)];			\
    }while(0)

#define INSTRUCTION(n,x)			\
  n:						\
  fprintf(stderr, #n "\n");			\
  x;						\
  DO_DUMP(stderr);				\
  NEXT

#endif

/* 
   likely and unlikely are hints to GCC to help it arrange
   if-then-else statements so that the common path is faster.  We use
   these to help optimize type assertions in the interpreter.
*/
#define unlikely(x)     __builtin_expect((x),0)
#define likely(x)       __builtin_expect((x),1)

#define ASSERT_TYPE(x,t)     if(unlikely(CELL_TYPE(x) != t)){TYPE_ERROR(t);}
#define ASSERT_NOT_TYPE(x,t) if(unlikely(CELL_TYPE(x) == t)){TYPE_ERROR(!t);}
#define ASSERT_INS(x)        if(unlikely(!IS_INS(x))){TYPE_ERROR(INSTRUCTION);}

/* 
   HEAP_CHECK is called before allocations to ensure that we don't
   overflow.  If the allocation is too big, we invoke the garbage
   collector. If collecting garbage doesn't free up enough memory to
   do the allocation, we crash.
*/
#define HEAP_CHECK(n) do{					\
    word __tmp = n;						\
    if(unlikely(hp + __tmp >= heap_base + heap_size)){		\
      heap_base = (heap_base == prog_end ?			\
		   upper_heap : prog_end);			\
      gc(heap_base);						\
      if(unlikely(hp + __tmp >= heap_base + heap_size)){	\
	fprintf(stderr, "Unable to allocate memory!\n");	\
	exit(-ENOMEM);						\
      }								\
    }								\
  }while(0);

/* 
   When a type assertion fails we let the user know and then give a
   backtrace.  The error message could be better, it prints out useful
   type names like (2 << 30) instead of "PTR".  But something is
   better than nothing.  Hopefully, we never see these anyway.
*/
#define TYPE_ERROR(t) do{					\
    fprintf(stderr, "Type error. Expected "#t"\n");		\
    DO_DUMP(stderr);						\
    exit(-1);							\
  }while(0);

/* 
   The garbage collector is a basic stop and copy collector.  It
   doesn't really try to do anything clever, and (temporarily) uses a
   rather obscene amount of memory (twice MEM_SIZE!).  

   The new_heap argument is either upper_heap or prog_end, whichever
   is not currently active.
*/
void gc(word *new_heap)
{
  word mappings[MEM_SIZE];
  word worklist[MEM_SIZE];
  word *work = worklist;
  int i;
  word tmp, val;
  memset(mappings, (word)-1, MEM_SIZE*sizeof(word));

  hp = new_heap;

#ifdef __GC_DEBUG__
  fprintf(stderr, 
	  "Entering gc with new_heap = %ld\n",
	  new_heap - memory);
#endif
  /* here's the deal,
     worklist is a stack of tree branches we need
     to walk
     mappings is a map from the old ptr target (offset in 
         the memory array) to a pointer in the new heap
	 to which the data must be copied.

     The way this works is whenever we encounter a pointer
     we check two things:
        - is its entry in the mappings array equal to 
	  0xffffffff, if so then we have not yet allocated
	  space for it in the new heap.

	- is the target of the pointer inside the original
	  program code? if so, we do not copy it. Instead
	  we just set the entry in mappings to point to the
	  original target.  However, we must still add this 
	  cell to the worklist so that we walk its children.

     If a cell must be copied we first set up its reverse
     mapping based on the current heap pointer, increment
     the heap pointer by the size of the pointer target, 
     then add the original pointer to the worklist.  

     We initialize the worklist by looking at the root
     register and the stack.  

     We are finished when the worklist is empty.

     We start by allocating space in the new heap for the target of
     the root register (if it contains a pointer), and adding it to
     the work list.
  */
  if(CELL_TYPE(rr) == PTR){  
    *work = rr;
    work++;
    if(&memory[PTR_TARGET(rr)] >= prog_end){
#ifdef __GC_DEBUG__
      fprintf(stderr, "Copying target of root register %d -> %ld (sz: %d)\n",
	      PTR_TARGET(rr), hp - memory, PTR_SIZE(rr));
#endif
      rr = mappings[PTR_TARGET(rr)] = MAKE_PTR(hp-memory, PTR_SIZE(rr));
      hp += PTR_SIZE(rr);
    }else{
#ifdef __GC_DEBUG__
      fprintf(stderr, "Root register has target in prog_text: %d (sz: %d)\n",
	      PTR_TARGET(rr), PTR_SIZE(rr));
#endif
      mappings[PTR_TARGET(rr)] = rr;
    }
  }

  /*
    Now we scan through the stack and do essentially the same thing we
    did for the root register.
  */
  for(i=0;i<STACK_HEIGHT();i++){
    if(CELL_TYPE(STACK(i)) == PTR){
      if(mappings[PTR_TARGET(STACK(i))] == 0xffffffff){ 
	/* We haven't looked at this pointer target before. So add it
	   to the work queue and (if needed) allocate space. */
	*work = STACK(i);
	work++;
	if(&memory[PTR_TARGET(STACK(i))] >= prog_end){
#ifdef __GC_DEBUG__
	  fprintf(stderr, "Copying pointer in cell STACK(%d) from %d -> %ld (sz: %d)\n",
		  i, PTR_TARGET(STACK(i)), hp - memory, PTR_SIZE(STACK(i)));
#endif
	  STACK(i) = mappings[PTR_TARGET(STACK(i))] = MAKE_PTR(hp-memory, PTR_SIZE(STACK(i)));
	  hp += PTR_SIZE(STACK(i));
	}else{
#ifdef __GC_DEBUG__
	  fprintf(stderr, "Cell STACK(%d) has target in prog_text: %d (sz: %d)\n",
		  i, PTR_TARGET(STACK(i)), PTR_SIZE(STACK(i)));
#endif
	  mappings[PTR_TARGET(STACK(i))] = STACK(i);
	}
      }else{
	/* We've already handled another reference to the same target,
	   so we can just update the value on the stack */
	STACK(i) = mappings[PTR_TARGET(STACK(i))];
      }
    }
  }

  /*
    ok, the work list has been initialized, we now walk down the list
    copying the contents of each targeted cell as we go.  If we find a
    pointer, we perform the operations described above (allocation in
    new heap, initializing the mapping, and adding to the worklist)
    then write the new pointer-value into the target cell.
  */
  while(work > worklist){
    work--;
    tmp = *work;

    for(i=0;i<PTR_SIZE(tmp);i++){
      val = memory[PTR_TARGET(tmp)+i];
      if(CELL_TYPE(val) == PTR){
	if(mappings[PTR_TARGET(val)] == 0xffffffff){
	  /* the cell is a pointer we have not yet created a mapping
	     for.  That means we must add it to the work list, and if
	     it is above the program text, allocate space in the new
	     heap */
	  *work = val;
	  work++;
	  if(&memory[PTR_TARGET(val)] >= prog_end){
#ifdef __GC_DEBUG__
	    fprintf(stderr, 
		    "While copying ptr: %d -> %d (sz: %d) offset %d is a fresh pointer.\n"
		    "\tOriginal: %d sz: %d\n"
		    "\tFresh:    %ld\n",
		    PTR_TARGET(tmp),
		    PTR_TARGET(mappings[PTR_TARGET(tmp)]),
		    PTR_SIZE(tmp),
		    i,
		    PTR_TARGET(val), PTR_SIZE(val),
		    hp - memory);
#endif
	    /* set the value of the cell we're copying to be the new pointer */
	    memory[PTR_TARGET(mappings[PTR_TARGET(tmp)]) + i] = 
	      mappings[PTR_TARGET(val)] = MAKE_PTR(hp - memory, PTR_SIZE(val));
	    /* update the heap pointer */
	    hp += PTR_SIZE(val);
	  }else{
#ifdef __GC_DEBUG__
	    fprintf(stderr, 
		    "While copying ptr: %d -> %d (sz: %d) offset %d is inside prog txt.\n"
		    "\tOriginal: %d sz: %d\n",
		    PTR_TARGET(tmp),
		    PTR_TARGET(mappings[PTR_TARGET(tmp)]),
		    PTR_SIZE(tmp),
		    i,
		    PTR_TARGET(val), PTR_SIZE(val));
#endif
	    /* the target of this pointer is in the program text just
	       perform a straight copy and set the value for this cell in
	       the mappings to the identity map */
	    memory[PTR_TARGET(mappings[PTR_TARGET(tmp)]) + i] = mappings[PTR_TARGET(val)] = val;
	  }
	}else{
#ifdef __GC_DEBUG__
	  fprintf(stderr,
		  "While copying ptr: %d -> %d (sz: %d) offset %d is a familiar pointer.\n"
		  "\tOriginal: %d sz: %d\n"
		  "\tMapping:  %d\n",
		  PTR_TARGET(tmp),
		  PTR_TARGET(mappings[PTR_TARGET(tmp)]),
		  PTR_SIZE(tmp),
		  i,
		  PTR_TARGET(val), PTR_SIZE(val),
		  PTR_TARGET(mappings[PTR_TARGET(val)]));
#endif
	  /* this is a pointer, but we've seen it already.
	     we create a new pointer cell based on the value in the
	     mappings[] array */
	  memory[PTR_TARGET(mappings[PTR_TARGET(tmp)]) + i] = mappings[PTR_TARGET(val)];
	}
      }else{
	/* the cell isn't a pointer, we can just copy it */
	memory[PTR_TARGET(mappings[PTR_TARGET(tmp)]) + i] = val;
      }
    }
  }

  /* Now scan through the original program text and update any heap
     pointers to their new location */
  for(i=0;i<prog_end-memory;i++){
    if(CELL_TYPE(memory[i]) == PTR && mappings[PTR_TARGET(memory[i])] != 0xffffffff){
#ifdef __GC_DEBUG__
      if(mappings[PTR_TARGET(memory[i])] != memory[i]){
	fprintf(stderr, "Found stale pointer in program text at offset %d: %d updating to %d (sz: %d)\n",
		i, PTR_TARGET(memory[i]), PTR_TARGET(mappings[PTR_TARGET(memory[i])]), 
		PTR_SIZE(mappings[PTR_TARGET(memory[i])]));
      }
#endif
      memory[i] = mappings[PTR_TARGET(memory[i])];
    }
  }

#ifdef __GC_DEBUG__
  memset(new_heap == upper_heap ? prog_end : upper_heap, 0xabababab, heap_size*sizeof(word));
  fprintf(stderr, "After gc %ld cells available (hp = %ld)\n",
	  heap_size - (hp - heap_base), hp - memory);
#endif
}

/* 
   Ok, we're done with the quick and dirty garbage collector.  Before
   we implement the interpreter itself, we'll do a little interactive
   inspector. Someday, maybe we'll add breakpointing support.
*/
static inline void inspector(void){
    int  cmd;
    fprintf(stderr, ">> ");
    fflush(stderr);
    while( ((cmd = getchar()) != '\n')){
	switch(cmd){
	case 'c': while(getchar() != '\n'); single_step = 0; return;
	case 'p': {
	    int addr;
	    scanf("%d", &addr); 
	    while(getchar()!='\n');
	    fprintf(stderr, "%d:\t", addr);
	    print_cell(stderr, memory[addr]);
	    fprintf(stderr, "\n");
	} break;
	case 's': {
	    int offset;
	    scanf("%d", &offset); 
	    while(getchar()!='\n');
	    fprintf(stderr, "STACK(%d):\t", offset);
	    print_cell(stderr, STACK(offset));
	    fprintf(stderr, "\n");
	} break;
	case 'r': {
	    while(getchar()!='\n');
	    fprintf(stderr, "RR:\t");
	    print_cell(stderr, rr);
	    fprintf(stderr, "\n");
	} break;
	case 'd': {
	    while(getchar()!='\n');
	    DO_DUMP(stderr);
	} break;
	case 'n': {
	    while(getchar()!='\n');
	    single_step = 1;
	    return;
	}
	default: while(getchar()!='\n');
	    fprintf(stderr, 
		    "Unknown command '%c':\n"
		    "\tc\tcontinue\n"
		    "\tp <addr>\tprint address\n"
		    "\ts <off>\tprint stack value\n"
		    "\tr\tprint root register\n", cmd);
	    break;
	}
	fprintf(stderr, ">> ");
	fflush(stderr);
    }
}

#define MAX_BREAKPOINTS 32
struct breakpoint{
    word pc;
    word instr;
};
    
int main(int argc, char *argv[])
{
    /* The long awaited array of instructions! */
    static void* instructions[] = {
	[I_PUSH]	= &&PUSH,  [I_POP]  = &&POP,
	[I_SWAP]	= &&SWAP,  [I_DUP]  = &&DUP, 
	[I_ROT]		= &&ROT,

	/* Control flow */
	[I_CALL]	= &&CALL,
	[I_RET]		= &&RET,   [I_JMP]  = &&JMP,  
	[I_JTRUE]	= &&JTRUE, [I_END]  = &&END,
    
	/* Arithmetic */
	[I_ADD] 	= &&PLUS,  [I_MUL]  = &&MUL, 
	[I_SUB]		= &&SUB,   [I_DIV]  = &&DIV,
	[I_MOD]		= &&MOD,   [I_SHL]  = &&SHL,  
	[I_SHR]		= &&SHR,   [I_BOR]  = &&BOR,  
	[I_BAND]	= &&BAND,
    
	/* Comparison */
	[I_EQ]		= &&EQ,    [I_LT]   = &&LT,

	/* Reading and writing memory */
	[I_STOR]	= &&STOR,  [I_LOAD] = &&LOAD, [I_ALOC] = &&ALOC,
    
	/* I/0 */
	[I_GETC]	= &&GETC,  [I_DUMP] = &&DUMP, 
	[I_PINT]	= &&PINT,  [I_PCHR] = &&PCHR,

	/* Root Register manipulation */
	[I_RDRR]	= &&RDRR,  [I_WTRR] = &&WTRR,

	/* Type checking */
	[I_ISNUM]	= &&ISNUM, [I_ISLCONST] = &&ISLCONST,
	[I_ISPTR]	= &&ISPTR, [I_ISBOOL]   = &&ISBOOL,
	[I_ISCHR]	= &&ISCHR, [I_ISINS]    = &&ISINS,

	[I_PBIN]       = &&PBIN,
	[I_PBLCONSTI]   = &&PBLCONSTI,
	[I_PBVCONSTI]   = &&PBVCONSTI,
	[I_PBPTRI]      = &&PBPTRI,

	[I_BRK]         = &&BRK
    };

  /* We first do some basic startup stuff to load the program */
  int fd=-1;
  FILE *program_input_file=NULL;
  int nread;

  struct breakpoint breakpoints[MAX_BREAKPOINTS];
  int nr_breakpoints = 0;
  int tmp;
  for(tmp = 1; tmp < argc; tmp++){
      if(argv[tmp][0] == '-' && argv[tmp][1] == 'b'){
	  if(nr_breakpoints == MAX_BREAKPOINTS){
	      fprintf(stderr, "Error: maximum breakpoints exceeded\n");
	      exit(1);
	  }
	  if(argv[tmp][2] != '\0'){
	      breakpoints[nr_breakpoints].pc = atoi(&argv[tmp][2]);
	  }else if(tmp + 1 < argc){
	      tmp++;
	      breakpoints[nr_breakpoints].pc = atoi(argv[tmp]);
	  }else{
	      fprintf(stderr, "Error: -b must be followed by program counter\n");
	      exit(1);
	  }
	  nr_breakpoints++;
      }else if(fd < 0){
	  fd = open(argv[tmp], O_RDONLY);
	  if(fd < 0){
	      fprintf(stderr, "Failed to open program file '%s'\n", argv[tmp]);
	      exit(1);
	  }
      }else if(program_input_file == NULL){
	  program_input_file = fopen(argv[tmp], "r");
	  if(program_input_file == NULL){
	      fprintf(stderr, "Failed to open program input file '%s'\n", argv[tmp]);
	      exit(1);
	  }
      }else{
	  fprintf(stderr, "Error: unexpected argument '%s'\n", argv[tmp]);
	  exit(1);
      }
  }
  if(fd < 0){
      fd = STDIN_FILENO;
  }
  if(program_input_file == NULL){
      program_input_file = stdin;
  }
  /* read in as much as we can! */
  nread      = read(fd, memory, MEM_SIZE*sizeof(word));
  if(nread < 0){
    printf("Read failed\n");
    exit(1);
  }
  /* scale to words instead of bytes */
  nread /= 4;

  /* Initialize the global variables pointing to the end of the text
     and the heap pointer */
  prog_end   = heap_base = hp = memory + nread;

  /* Give half the remaining memory to each heap */
  heap_size  = (MEM_SIZE - STACK_SIZE - nread)/2;
  upper_heap = hp + heap_size;

  /* 
     The assembler (included in the tarball) outputs everything in
     network byte order so sweep through and fix everything for the
     native machine.
  */
  for(; nread > 0 ; nread--){
    memory[nread-1] = ntohl(memory[nread-1]);
  }

  /*
    Apply breakpoints, replace program locations specified on the
    commandline with BRK instructions.
  */
  for(tmp = 0;tmp < nr_breakpoints;tmp++){
      if(breakpoints[tmp].pc < (prog_end - memory)){
	  breakpoints[tmp].instr = memory[breakpoints[tmp].pc];
	  memory[breakpoints[tmp].pc] = I_BRK;
      }
  }
  
  /* pc points to the first cell of memory which is the first opcode
     to execute.  All we have to do to kick things off is call the
     NEXT macro to jump to that instruction handler and increment the
     pc! */
  NEXT;

  /* Now we just have to implement our instructions.  These all use
     the INSTRUCTION macro defined above.  Most of them are fairly
     straightforward */

  /* stack manipulation: push, pop, swap, and rotate */
  INSTRUCTION(PUSH, STACK_PUSH(*pc++));
  INSTRUCTION(POP, ignore(STACK_POP()));
  INSTRUCTION(SWAP,
	      do{
		word tmp = STACK(0);
		STACK(0) = STACK(1);
		STACK(1) = tmp;
	      }while(0)
	      );
  INSTRUCTION(DUP, STACK_PUSH(STACK(0)));
  INSTRUCTION(ROT,
	      do{
		word tmp = STACK(0);		
		STACK(0) = STACK(1);
		STACK(1) = STACK(2);
		STACK(2) = tmp;
	      }while(0)
	      );

  /* Flow control */
  /* Function calls are really primitive: pop the destination address
     off the stack, push the current pc on, then set the new pc. */
  INSTRUCTION(CALL,
	      do{
		word tmp = pc - memory;
		ASSERT_TYPE(STACK(0), PTR);
		if(callstack_top < &callstack[255]){
		    *callstack_top = pc - memory;
		    callstack_top++;
		    *callstack_top = PTR_TARGET(STACK(0));
		    callstack_top++;
		}
		pc = memory + PTR_TARGET(STACK(0));
		STACK(0) = MAKE_PTR(tmp,0);
	      }while(0)
	      );

  /* Returning is basically the opposite of calling (as you'd expect).
     The exception is that the top of stack is treated as the function
     return value, so we restore the pc from the second to top stack
     element, then copy the top element over it and pop. */
  INSTRUCTION(RET,
	      do{
		ASSERT_TYPE(STACK(1), PTR);
		pc = memory + PTR_TARGET(STACK(1));
		if(callstack_top > callstack){
		    callstack_top--;
		    callstack_top--;
		}
		STACK(1) =  STACK(0);
		ignore(STACK_POP());
	      }while(0)
	      );

  /* The target of an unconditional jump is the top stack element.
     All jumps are absolute. */
  INSTRUCTION(JMP, ASSERT_TYPE(STACK(0), PTR); 
	      pc = memory + PTR_TARGET(STACK_POP()));

  /* Conditional jumping: top element is the destination, next element
     is a boolean indicating whether or not to jump. */
  INSTRUCTION(JTRUE,  
	      do{
		ASSERT_TYPE(STACK(0), PTR);
		if(unlikely(!IS_BOOL(STACK(1)))){TYPE_ERROR(BOOL);}
		pc = ((STACK(1) ==  TRUE_VAL) ?
		      memory + PTR_TARGET(STACK(0)) : pc);
		ignore(STACK_POP()); 
		ignore(STACK_POP());
	      }while(0)
	      );

  /* END is used to stop the interpreter, just exit the main
     routine. */
  INSTRUCTION(END, 
	      return 0);

  /* arithmetic */
  /* Note that for PLUS and MUL the NUM_TO_NATIVE conversions are not
     really needed since the flag bits will just overflow and get
     reset. We do them anyway just for the sake of being explicit. */
  INSTRUCTION(PLUS, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) +  
				  NUM_TO_NATIVE(STACK(0)));
	      ignore(STACK_POP()));
    
  INSTRUCTION(MUL,
	      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) * 
				  NUM_TO_NATIVE(STACK(0))); 
	      ignore(STACK_POP()));

  INSTRUCTION(SUB, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) -  
				  NUM_TO_NATIVE(STACK(0)));
	      ignore(STACK_POP()));

  INSTRUCTION(DIV, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) / 
				  NUM_TO_NATIVE(STACK(0)));
	      ignore(STACK_POP()));

  INSTRUCTION(MOD, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) %  
				  NUM_TO_NATIVE(STACK(0)));
	      ignore(STACK_POP()));

  /* Bitwise operations. */
  INSTRUCTION(SHL, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) << NUM_TO_NATIVE(STACK(0)));
      ignore(STACK_POP());
    }while(0));

  INSTRUCTION(SHR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) >> NUM_TO_NATIVE(STACK(0)));
      ignore(STACK_POP());
    }while(0));

  INSTRUCTION(BOR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) | NUM_TO_NATIVE(STACK(0)));
      ignore(STACK_POP());
    }while(0));

  INSTRUCTION(BAND, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) & NUM_TO_NATIVE(STACK(0)));
      ignore(STACK_POP());
    }while(0));
  
  /* Comparison */
  /* Note that EQ is type agnostic where as LT requires its arguments
     are integers. */
  INSTRUCTION(EQ, do{
      STACK(1) = (STACK(0) == STACK(1) ? TRUE_VAL : FALSE_VAL); 
      ignore(STACK_POP());
    }while(0));

  INSTRUCTION(LT, do{
	  if(IS_NUM(STACK(0)) && IS_NUM(STACK(1))){
	      STACK(1) = NUM_TO_NATIVE(STACK(0)) < NUM_TO_NATIVE(STACK(1)) ? TRUE_VAL : FALSE_VAL;	     
	  }else if(IS_CHAR(STACK(0)) && IS_CHAR(STACK(1))){
	      STACK(1) = CHAR_TO_NATIVE(STACK(0)) < CHAR_TO_NATIVE(STACK(1)) ? TRUE_VAL : FALSE_VAL;
	  }else{
	      TYPE_ERROR(CHAR or NUM);
	  }
	  ignore(STACK_POP());
      }while(0));
  
  /* Memory access */

  /* Since we don't allow explicit arithmetic on pointers the load and
     store operations use a base pointer (second to top element of
     stack) and an integer offset (top element of stack).  We use the
     size field of the pointer to ensure memory safety. 

     For stores,  the value to store is the third thing on the stack. */
  INSTRUCTION(STOR, do{
      ASSERT_TYPE(STACK(0), NUM)
	ASSERT_TYPE(STACK(1), PTR);
      if(NUM_TO_NATIVE(STACK(0)) < 0 || 
	 NUM_TO_NATIVE(STACK(0)) >= PTR_SIZE(STACK(1))){
	fprintf(stderr, "Invalid store: offset %"PRId32" out of bounds\n",
		NUM_TO_NATIVE(STACK(0)));
	DO_DUMP(stderr);
	exit(1);
      }
      memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))] = STACK(2);
      STACK(2) = STACK(1); /* leave the pointer on the top of the stack */
      ignore(STACK_POP());
      ignore(STACK_POP());
    }while(0)
    );

  INSTRUCTION(LOAD, do{
      ASSERT_TYPE(STACK(0), NUM);
      ASSERT_TYPE(STACK(1), PTR);
      if(NUM_TO_NATIVE(STACK(0)) < 0 || 
	 NUM_TO_NATIVE(STACK(0)) > PTR_SIZE(STACK(1))){
	fprintf(stderr, "Invalid load: offset %"PRId32" out of bounds\n", 
		NUM_TO_NATIVE(STACK(0)));
	DO_DUMP(stderr);
	exit(1);
      }
      STACK(1) = memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))];
      ignore(STACK_POP());
    }while(0));

  /* Memory Allocation */
  INSTRUCTION(ALOC, do{
	  word __tmp;
	  word __sz;
	  ASSERT_TYPE(STACK(0), NUM);
	  __sz = NUM_TO_NATIVE(STACK(0));
	  if(__sz > MAX_PTR_SIZE){
	      fprintf(stderr, "Requested allocation %d too large.", __sz);
	      exit(1);
	  }
	  HEAP_CHECK(__sz);
	  __tmp = MAKE_PTR(hp - memory, __sz);
	  hp += __sz;
	  STACK(0) = __tmp;
      }while(0));
  
  /* I/O */
  INSTRUCTION(GETC, STACK_PUSH(MAKE_CHAR(fgetc(program_input_file))));
  INSTRUCTION(DUMP, DO_DUMP(stdout));
  INSTRUCTION(PINT, ASSERT_TYPE(STACK(0), NUM);    
	      printf("%"PRId32, NUM_TO_NATIVE(STACK(0))); 
	      ignore(STACK_POP()));
  INSTRUCTION(PCHR, ASSERT_TYPE(STACK(0), VCONST); 
	      putchar(CHAR_TO_NATIVE(STACK_POP())));

  /* Root register manipulation */
  INSTRUCTION(RDRR, STACK_PUSH(rr));
  INSTRUCTION(WTRR, rr = STACK_POP());

  /* Type checking */
  INSTRUCTION(ISNUM,    STACK(0) = (IS_NUM(STACK(0)))    ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISLCONST, STACK(0) = (IS_LCONST(STACK(0))) ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISPTR,    STACK(0) = (IS_PTR(STACK(0)))    ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISBOOL,   STACK(0) = (IS_BOOL(STACK(0)))   ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISCHR,    STACK(0) = (IS_CHAR(STACK(0)))   ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISINS,    STACK(0) = (IS_INS(STACK(0)))    ? TRUE_VAL : FALSE_VAL);

  INSTRUCTION(PBIN, do{
	  word w = htonl(STACK(0));
	  fwrite(&w, sizeof(w), 1, stdout);
	  ignore(STACK_POP());
      }while(0));
  INSTRUCTION(PBLCONSTI, do{
	  ASSERT_TYPE(STACK(0), NUM);
	  word w = htonl(MAKE_LCONST(NUM_TO_NATIVE(STACK(0))));	      
	  fwrite(&w, sizeof(w), 1, stdout);
	  ignore(STACK_POP());
      }while(0));
  INSTRUCTION(PBVCONSTI, do{
	  ASSERT_TYPE(STACK(0), NUM);
	  word w = htonl(MAKE_VCONST(NUM_TO_NATIVE(STACK(0))));	      
	  fwrite(&w, sizeof(w), 1, stdout);
	  ignore(STACK_POP());
      }while(0));
  INSTRUCTION(PBPTRI, do{
	  ASSERT_TYPE(STACK(0), NUM);
	  ASSERT_TYPE(STACK(1), NUM);
	  word base = NUM_TO_NATIVE(STACK(0));
	  word size = NUM_TO_NATIVE(STACK(1));
	  word ptr  = htonl(MAKE_PTR(base, size));
	  fwrite(&ptr, sizeof(ptr), 1, stdout);
	  ignore(STACK_POP());
	  ignore(STACK_POP());
      }while(0));

  INSTRUCTION(BRK, do{
	  int brknum;
	  inspector();
	  for(brknum = 0; brknum < nr_breakpoints; brknum++){
	      if(breakpoints[brknum].pc == ((pc-1) - memory)){
		  goto *instructions[breakpoints[brknum].instr];
	      }
	  }
	  fprintf(stderr, "Error: failed to continue from breakpoint at %ld\n", pc - memory);
	  DO_DUMP(stderr);
	  exit(1);
      }while(0));
  return 0;
}
  
/*
  And we're done.  Next time will be either a compiler for a simple
  language to this interpreter (hopefully a better compiler than the
  one in the tarball), or some form of JIT add-on to the interpreter.
*/
