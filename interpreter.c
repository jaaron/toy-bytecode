/*
  Bytecode virtual machines are a popular topic these days.  The
  popularity of high level dynamic languages makes improvements in all
  things bytecode big news. Some of my favorite projects to track are

       PyPy http://codespeak.net/pypy/
       Tamarin Tracing https://wiki.mozilla.org/Tamarin:Tracing
       and SquirrelFish http://trac.webkit.org/wiki/SquirrelFish

   I think the coolest area of research right now is in JIT (just in
   time) compilers.  But first, let's write an interpreter.

   This post is in good old fashioned C.  The complete source code for
   the interpreter, an assembler, a pretty hackish compiler, and some
   examples can be downloaded from 
   http://aaron.13th-floor.org/src/bytecode.tgz
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

   All types used by the VM fit in a 32 bit word (throughout the code
   we assume native word size is 32 bits.  porting to 64 bits is left
   as an exercise to the reader), the top two bits are reserved for
   flags used to identify the type of the word.

   Pointers are are packed to include a 15 bit size field, and a 15
   bit location pointer (the total memory size is also defined in
   interpreter.h to be 8192).

   VM constants include instructions, characters, and the boolean
   values true and false.  

   Languages are free to define up to 2^30 new constants that will be 
   treated as opaque blobs by the machine (only an equality operation is
   defined).
*/

#include "interpreter.h"

/* 
   Our interpreter will be a basic stack machine.  Memory consists of
   a large array with the stack at the top growing down, the static
   code is at the bottom, then two heaps (one live, and one dead used
   for garbage collection).  The heap grows monotonically upward until
   it is about to collide either with the other heap, or the reserved
   stack space (depending on which heap is live) at which point the
   garbage collector is invoked.
*/

/*
   Sidenote: if you download the tarball from my website you'll notice
   a number of different definitions of the following macros that can
   be selected using various preprocessor defines.  These include
   versions that do additional bounds checking on stack
   access/allocation, and dumping VM status after each instruction.
   For the sake of brevity (well, relative brevity) I'm only including
   the basic versions in this post.
*/

#ifdef __GUARD_STACK__
#define STACK(x) ({					\
  int32_t __tmp = x;					\
  if(__tmp > STACK_HEIGHT()){				\
  fprintf(stderr, "Invalid stack access at %ld\n",	\
	  pc-memory);					\
  exit(-1);						\
  }							\
  *(sp + 1 + __tmp);					\
    })

#define STACK_POP() if(!STACK_HEIGHT()){			\
  fprintf(stderr, "Attempt to pop empty stack at %ld\n",	\
	  pc-memory);						\
  }else{							\
  ++sp;								\
  }

#else

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

#endif

/* 
   Pushing an element on the stack.  Like popping this changes the
   stack pointer so use with care.
*/
#define STACK_PUSH(x) do{			\
    int32_t __tmp = x;				\
    *sp = __tmp;				\
    sp--;					\
  }while(0)
#define STACK_HEIGHT() ((memory + MEM_SIZE-1) - sp)


/* 
   The following macro is used to dump the current machine state (pc,
   heap pointer, stack pointer, and stack contents) to an output
   stream.  It's mostly intended for debugging purposes and bailing
   out on type errors.
*/
#define DO_DUMP(stream) do{						\
    int q=0;								\
    fprintf(stream, "pc: %ld, hp: %ld sp: %p height: %ld\nstack:\n",	\
	    pc-memory, hp-memory, sp, STACK_HEIGHT());			\
    while(q < STACK_HEIGHT() ){						\
      switch(CELL_TYPE(STACK(q))){					\
      case NUM:								\
	  fprintf(stream, "\t%"PRId32" (%"PRIx32")\n",			\
		  NUM_TO_NATIVE(STACK(q)), STACK(q));			\
	break;								\
      case VCONST:							\
	if(IS_CHAR(STACK(q))){						\
	  fprintf(stream,"\tchar: %c\n", CHAR_TO_NATIVE(STACK(q)));	\
	}else if(IS_BOOL(STACK(q))){					\
	  fprintf(stream, "\tbool: %c\n",				\
		  STACK(q) == TRUE_VAL ? 'T' : 'F');			\
	}else{								\
	    fprintf(stream, "\tvc: %0"PRIx32"\n", STACK(q));		\
	}								\
	break;								\
      case LCONST:							\
	  fprintf(stream, "\tlc: %"PRId32"\n",				\
		  NUM_TO_NATIVE(STACK(q)));				\
	break;								\
      case PTR:								\
	fprintf(stream, "\tptr: %d sz: %d\n",				\
		PTR_TARGET(STACK(q)), PTR_SIZE(STACK(q)));		\
	break;								\
      default:								\
	  fprintf(stream, "\t(%1x)?: %0"PRIx32"\n",			\
		  CELL_TYPE(STACK(q)), STACK(q) & (~(0x3 << 30)));	\
      }									\
      q++;								\
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

#ifdef __TRACE__

#define NEXT do{						\
    fprintf(stderr,"==================================\n");	\
    fprintf(stderr,"initial pc: %ld instruction: %"PRId32" ",	\
	    pc-memory, IDX_FROM_INS(*pc));			\
    goto *instructions[IDX_FROM_INS(*pc++)];			\
  }while(0)

#define INSTRUCTION(n,x)			\
  n:						\
  fprintf(stderr, #n "\n");			\
  x;						\
  DO_DUMP(stderr);				\
  NEXT

#else 

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

/* 
   As implied above, instruction handlers have to have a specific
   format for everything to work nicely. We're going to use gcc's
   computed goto feature to build our array, so each instruction will
   start with a label naming the instruction, then some C code
   implementing the handler, then a call to NEXT.
*/
#define INSTRUCTION(n,x)			\
  n:						\
  x;						\
  NEXT
#endif

/* 
   likely and unlikely are hints to GCC to help it arrange
   if-then-else statements so that the common path is faster.  We use
   these to help optimize type assertions in the interpreter.
*/
#define unlikely(x)     __builtin_expect((x),0)
#define likely(x)     __builtin_expect((x),1)

/* 
   Now come a series of macros for checking and unpacking the
   different VM types.  The corresponding packers are defined in the
   interpreter.h header file.
*/
#define PTR_TARGET(x)        (((unsigned int)x) & 0x7fff)
#define PTR_SIZE(x)          ((((unsigned int)x) >> 15) & 0x7ff)
#define NUM_TO_NATIVE(x)     ((typeof(x))((((int)x) << 2) >> 2))
#define CHAR_TO_NATIVE(x)    ((char)((x) & 0xff))
#define CELL_TYPE(x)         (((unsigned int)x) & (0x3 << 30))
#define IS_NUM(x)            (CELL_TYPE(x) == NUM)
#define IS_PTR(x)            (CELL_TYPE(x) == PTR)
#define IS_LCONST(x)         (CELL_TYPE(x) == LCONST)
#define IS_VCONST(x)         (CELL_TYPE(x) == VCONST)
#define IS_CONST(x)          (IS_LCONST(x)  || IS_VCONST(x))
#define IS_BOOL(x)           (x == TRUE_VAL || x == FALSE_VAL)
#define IS_CHAR(x)           (IS_VCONST(x) && ((x) & CHAR_FLAG) == CHAR_FLAG)
#define IS_INS(x)            (IS_VCONST(x) && ( (x) < NR_INS))

#define ASSERT_TYPE(x,t)     if(unlikely(CELL_TYPE(x) != t)){TYPE_ERROR(t);}
#define ASSERT_NOT_TYPE(x,t) if(unlikely(CELL_TYPE(x) == t)){TYPE_ERROR(!t);}

/* 
   HEAP_CHECK is called before allocations to ensure that we don't
   overflow.  If the allocation is too big, we invoke the garbage
   collector. If collecting garbage doesn't free up enough memory to
   do the allocation, we crash.
*/
#define HEAP_CHECK(n) do{					\
    long __tmp = n;						\
    if(hp + __tmp >= heap_base + heap_size){			\
      heap_base = (heap_base == prog_end ?			\
		   upper_heap : prog_end);			\
      gc(heap_base);						\
      if(hp + __tmp >= heap_base + heap_size){			\
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

static int32_t memory[MEM_SIZE];          /* all of memory */

static int32_t *sp = &memory[MEM_SIZE-1]; /* The top 256 words of memory
					  are reserved for the stack */

static int32_t *prog_end;                 /* pointer to the end of the
					  loaded program text */

static int32_t *pc = memory;              /* the program counter */

static int32_t heap_size;                 /* maximum size of a heap
					  (computed based on
					  prog_end) */

static int32_t *upper_heap;               /* upper_heap == prog_end + heap_size; */

static int32_t *heap_base;                /* pointer to the first cell of
					  the heap, Should always be
					  either prog_end, or
					  upper_head. */
				       
static int32_t *hp;                       /* the heap pointer. */

static int32_t  rr = MAKE_NUM(0);         /* the root regiseter can be
					  used by the language to
					  store an additional root for
					  gc (anything on the stack is
					  also treated as a root).
					  The intended use is as a
					  pointer to a structure
					  containing local variables. */

#ifdef __STATS__
static int32_t max_alloc = 0;
#endif

/* 
   The garbage collector is a basic stop and copy collector.  It
   doesn't really try to do anything clever, and (temporarily) uses a
   rather obscene amount of memory (twice MEM_SIZE!).  

   The new_heap argument is either upper_heap or prog_end, whichever
   is not currently active.
*/
void gc(int32_t *new_heap)
{
  int32_t mappings[MEM_SIZE];
  int32_t worklist[MEM_SIZE];
  int32_t *work = worklist;
  int i;
  int32_t tmp, val;
  memset(mappings, 0xffffffff, MEM_SIZE*sizeof(int32_t));

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
      fprintf(stderr, "Copying target of root register %d -> %d (sz: %d)\n",
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
	  fprintf(stderr, "Copying pointer in cell STACK(%d) from %d -> %d (sz: %d)\n",
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

    if(PTR_SIZE(tmp) == 0xff) continue;

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
		    "\tFresh:    %d\n",
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
  memset(new_heap == upper_heap ? prog_end : upper_heap, 0xabababab, heap_size*sizeof(int32_t));
  fprintf(stderr, "After gc %d cells available (hp = %d)\n",
	  heap_size - (hp - heap_base), hp - memory);
#endif
}

/* 
   Ok, we're done with the quick and dirty garbage collector.  Let's
   actually implement the interpreter!
*/
int main(int argc, char *argv[])
{
  /* The long awaited array of instructions!  It is imperative that
     these appear in the order specified in interpreter.h */
  static long instructions[] = {
    (long)&&PUSH, (long)&&POP,
    (long)&&SWAP, (long)&&DUP, 
    (long)&&ROT,

    /* Control flow */
    (long)&&CALL,
    (long)&&RET,   (long)&&JMP,  
    (long)&&JTRUE, (long)&&END,
    
    /* Arithmetic */
    (long)&&PLUS, (long)&&MUL, 
    (long)&&SUB,  (long)&&DIV,
    (long)&&MOD,  (long)&&SHL,  
    (long)&&SHR,  (long)&&BOR,  
    (long)&&BAND,
    
    /* Comparison */
    (long)&&EQ,   (long)&&LT,

    /* Reading and writing memory */
    (long)&&STOR, (long)&&LOAD, (long)&&ALOC,
    
    /* I/0 */
    (long)&&GETC, (long)&&DUMP, 
    (long)&&PINT, (long)&&PCHR,

    /* Root Register manipulation */
    (long)&&RDRR, (long)&&WTRR,

    /* Type checking */
    (long)&&ISNUM, (long)&&ISLCONST,
    (long)&&ISPTR, (long)&&ISBOOL,
    (long)&&ISCHR, (long)&&ISINS
  };


  /* We first do some basic startup stuff to load the program */
  int fd;
  int nread;  
  /* Read from a file or stdin */
  fd = argc > 1 ? open(argv[1], O_RDONLY) : STDIN_FILENO;
  if(fd < 0){
    printf("Open failed\n");
    exit(1);
  }
  /* read in as much as we can! */
  nread      = read(fd, memory, MEM_SIZE*sizeof(long));
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

  /* The assembler (included in the tarball) outputs everything in
     network byte order (my main desktop is PPC) so sweep through and
     fix everything for the native machine.  We could skip this on
     PPC. 
  */
  for(; nread > 0 ; nread--){
    memory[nread-1] = ntohl(memory[nread-1]);
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
  INSTRUCTION(POP, STACK_POP());
  INSTRUCTION(SWAP,
	      do{
		int32_t tmp = STACK(0);
		STACK(0) = STACK(1);
		STACK(1) = tmp;
	      }while(0)
	      );
  INSTRUCTION(DUP, STACK_PUSH(STACK(0)));
  INSTRUCTION(ROT,
	      do{
		int32_t tmp = STACK(0);		
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
		int32_t tmp = pc - memory;
		ASSERT_TYPE(STACK(0), PTR);
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
		STACK(1) =  STACK(0);
		STACK_POP();
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
		STACK_POP(); 
		STACK_POP();
	      }while(0)
	      );

  /* END is used to stop the interpreter, just exit the main
     routine. */
  INSTRUCTION(END, 
#ifdef __STATS__
	      fprintf(stderr, "max_alloc = %d\n", max_alloc);
#endif
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
	      STACK_POP());
    
  INSTRUCTION(MUL,
	      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) * 
				  NUM_TO_NATIVE(STACK(0))); 
	      STACK_POP());

  INSTRUCTION(SUB, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) -  
				  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

  INSTRUCTION(DIV, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) / 
				  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

  INSTRUCTION(MOD, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) %  
				  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

  /* Bitwise operations. */
  INSTRUCTION(SHL, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) << NUM_TO_NATIVE(STACK(0)));
      STACK_POP();
    }while(0));

  INSTRUCTION(SHR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) >> NUM_TO_NATIVE(STACK(0)));
      STACK_POP();
    }while(0));

  INSTRUCTION(BOR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) | NUM_TO_NATIVE(STACK(0)));
      STACK_POP();
    }while(0));

  INSTRUCTION(BAND, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) & NUM_TO_NATIVE(STACK(0)));
      STACK_POP();
    }while(0));
  
  /* Comparison */
  /* Note that EQ is type agnostic where as LT requires its arguments
     are integers. */
  INSTRUCTION(EQ, do{
      STACK(1) = (STACK(0) == STACK(1) ? TRUE_VAL : FALSE_VAL); 
      STACK_POP();
    }while(0));

  INSTRUCTION(LT, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM); 
      STACK(1) = (STACK(0) < STACK(1) ? TRUE_VAL : FALSE_VAL); 
      STACK_POP();
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
	exit(1);
      }
      memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))] = STACK(2);
      STACK_POP();
      STACK_POP();
      STACK_POP();
    }while(0)
    );

  INSTRUCTION(LOAD, do{
      ASSERT_TYPE(STACK(0), NUM);
      ASSERT_TYPE(STACK(1), PTR);
      if(NUM_TO_NATIVE(STACK(0)) < 0 || 
	 NUM_TO_NATIVE(STACK(0)) > PTR_SIZE(STACK(1))){
	fprintf(stderr, "Invalid load: offset %"PRId32" out of bounds\n", 
		NUM_TO_NATIVE(STACK(0)));
	exit(1);
      }
      STACK(1) = memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))];
      STACK_POP();
    }while(0));

  /* Memory Allocation */
  INSTRUCTION(ALOC, do{
      int32_t __tmp;
      ASSERT_TYPE(STACK(0), NUM);
      HEAP_CHECK(NUM_TO_NATIVE(STACK(0)));
      __tmp = MAKE_PTR(hp - memory, NUM_TO_NATIVE(STACK(0)));
      hp += NUM_TO_NATIVE(STACK(0));
      STACK(0) = __tmp;
#ifdef __STATS__
      if(hp - heap_base > max_alloc){
	max_alloc = hp - heap_base;
      }
#endif
    }while(0));
  
  /* I/O */
  INSTRUCTION(GETC, STACK_PUSH(MAKE_CHAR(getchar())));
  INSTRUCTION(DUMP, DO_DUMP(stdout));
  INSTRUCTION(PINT, ASSERT_TYPE(STACK(0), NUM);    
	      printf("%"PRId32, NUM_TO_NATIVE(STACK(0))); STACK_POP());
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
  return 0;
}
  
/*
  And we're done.  Next time will be either a compiler for a simple
  language to this interpreter (hopefully a better compiler than the
  one in the tarball), or some form of JIT add-on to the interpreter.
*/
