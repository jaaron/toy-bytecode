#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include "interpreter.h"
#include <string.h>

/* Internal macros */
#ifdef __GUARD_STACK__
#define STACK(x) ({					\
  long __tmp = x;					\
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
#define STACK(x) *(sp+1+(x))

/* stack pop changes the stack pointer,  do 
   not use it in combination with any macro that 
   accesses the stack pointer.  */
#define STACK_POP() *( ++sp )

#endif

#define STACK_PUSH(x) do{			\
    long __tmp = x;				\
    *sp = __tmp;				\
    sp--;					\
  }while(0)
#define STACK_HEIGHT() ((memory + MEM_SIZE-1) - sp)

#define DO_DUMP(stream) do{						\
    int q=0;								\
    fprintf(stream, "pc: %d, hp: %d sp: %p height: %d\nstack:\n",	\
	    pc-memory, hp-memory, sp, STACK_HEIGHT());			\
    while(q < STACK_HEIGHT() ){						\
      switch(CELL_TYPE(STACK(q))){					\
      case NUM:								\
	fprintf(stream, "\t%d\n", NUM_TO_NATIVE(STACK(q)));		\
	break;								\
      case VCONST:							\
	if(IS_CHAR(STACK(q))){						\
	  fprintf(stream,"\tchar: %c\n", CHAR_TO_NATIVE(STACK(q)));	\
	}else if(IS_BOOL(STACK(q))){					\
	  fprintf(stream, "\tbool: %c\n",				\
		  STACK(q) == TRUE_VAL ? 'T' : 'F');			\
	}else{								\
	  fprintf(stream, "\tvc: %08lx\n", STACK(q));			\
	}								\
	break;								\
      case LCONST:							\
	fprintf(stream, "\tlc: %d\n", NUM_TO_NATIVE(STACK(q)));		\
	break;								\
      case PTR:								\
	fprintf(stream, "\tptr: %d sz: %d\n",				\
		PTR_TARGET(STACK(q)), PTR_SIZE(STACK(q)));		\
	break;								\
      default:								\
	fprintf(stream, "\t(%1x)?: %08lx\n",				\
		CELL_TYPE(STACK(q)), STACK(q) & (~(0x3 << 30)));	\
      }									\
      q++;								\
    }									\
  }while(0)

#define IDX_FROM_INS(i) (i)

#ifdef __TRACE__

#define NEXT do{						\
    fprintf(stderr,"==================================\n");	\
    fprintf(stderr,"initial pc: %d instruction: %d ",		\
	    pc-memory, IDX_FROM_INS(*pc));			\
    goto *instructions[IDX_FROM_INS(*pc++)];			\
  }while(0)

#define INSTRUCTION(n,x)			\
  n:						\
  fprintf(stderr, #n "\n");			\
  x;						\
  DO_DUMP(stderr);				\
  NEXT

#else /* quiet versions! */

#define NEXT goto *instructions[IDX_FROM_INS(*pc++)]
#define INSTRUCTION(n,x)			\
  n:						\
  x;						\
  NEXT
#endif

#define unlikely(x)     __builtin_expect((x),0)
#define likely(x)     __builtin_expect((x),1)

#define PTR_TARGET(x) (((unsigned int)x) & 0x7fff)
#define PTR_SIZE(x)   ((((unsigned int)x) >> 15) & 0x7ff)

#define NUM_TO_NATIVE(x) (((x) << 2) >> 2)

#define CHAR_TO_NATIVE(x) ((char)((x) & 0xff))

#define CELL_TYPE(x) (((unsigned int)x) & (0x3 << 30))

#define IS_NUM(x)    (CELL_TYPE(x) == NUM)
#define IS_PTR(x)    (CELL_TYPE(x) == PTR)
#define IS_LCONST(x) (CELL_TYPE(x) == LCONST)
#define IS_VCONST(x) (CELL_TYPE(x) == VCONST)
#define IS_CONST(x)  (IS_LCONST(x)  || IS_VCONST(x))
#define IS_BOOL(x)   (x == TRUE_VAL || x == FALSE_VAL)
#define IS_CHAR(x)   (IS_VCONST(x) && ((x) & CHAR_FLAG) == CHAR_FLAG)
#define IS_INS(x)    (IS_VCONST(x) && ( (x) < NR_INS))

#define ASSERT_TYPE(x,t)     if(unlikely(CELL_TYPE(x) != t)){TYPE_ERROR(t);}
#define ASSERT_NOT_TYPE(x,t) if(unlikely(CELL_TYPE(x) == t)){TYPE_ERROR(!t);}

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

#define TYPE_ERROR(t) do{					\
    fprintf(stderr, "Type error. Expected "#t"\n");		\
    DO_DUMP(stderr);						\
    exit(-1);							\
  }while(0);

static long memory[MEM_SIZE];          /* all of memory */
static long *sp = &memory[MEM_SIZE-1]; /* the stack grows down from the top 
					  (sp < &memory[MEM_SIZE] && 
					  sp > memory[MEM_SIZE-STACK_SIZE])
					*/
static long *prog_end;                 /* pointer to the end of the loaded program text */
static long *pc = memory;              /* the program counter */
static long heap_size;                 /* maximum size of a heap (computed based on prog_end) */
static long *upper_heap;               /* upper_heap == prog_end + heap_size; */
static long *heap_base;                /* pointer to the first cell of the heap, 
					  At all times
					  (heap_base == prog_end || heap_base == upper_heap) 
				       */
static long *hp;                       /* the heap pointer, 
					  At all times 
				          (heap_base <= hp && hp < heap_base + heap_size)
				       */
static long  rr = MAKE_NUM(0);         /* the root regiseter can be used by the language
					  to store an additional root for gc (anything 
					  on the stack is also treated as a root). */

#ifdef __STATS__
static long max_alloc = 0;
#endif

void gc(long *new_heap)
{
  long mappings[MEM_SIZE];
  long worklist[MEM_SIZE];
  long *work = worklist;
  int i;
  long tmp, val;
  memset(mappings, 0xffffffff, MEM_SIZE*sizeof(long));

  hp = new_heap;

#ifdef __GC_DEBUG__
  fprintf(stderr, 
	  "Entering gc with new_heap = %d\n",
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
        - is it's entry in the mappings array equal to 
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

  for(i=0;i<STACK_HEIGHT();i++){
    if(CELL_TYPE(STACK(i)) == PTR){
      if(mappings[PTR_TARGET(STACK(i))] == 0xffffffff){
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
	STACK(i) = mappings[PTR_TARGET(STACK(i))];
      }
    }
  }

  /*
    ok, the work list has been initialized, we now walk 
    down the list copying the contents of each targeted 
    cell as we go.  If we find a pointer, we perform the
    operations described above (allocation in new heap, 
    initializing the mapping, and adding to the worklist)
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
	  /* the cell is a pointer we have not yet 
	     created a mapping for.  That means we 
	     must add it to the work list, and if it
	     is above the program text, allocate space
	     in the new heap */
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
	    memory[PTR_TARGET(mappings[PTR_TARGET(tmp)]) + i] = mappings[PTR_TARGET(val)] = MAKE_PTR(hp - memory, PTR_SIZE(val));
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
	    /* the target of this pointer is in the program text */
	    /* just perform a straight copy and set the value for this */	       
	    /* cell in the mappings to the identity map */
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

  memset(new_heap == upper_heap ? prog_end : upper_heap, 0xabababab, heap_size*sizeof(long));


#ifdef __GC_DEBUG__
  fprintf(stderr, "After gc %d cells available (hp = %d)\n",
	  heap_size - (hp - heap_base), hp - memory);
#endif

}

int main(int argc, char *argv[])
{
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

  int fd;
  long nread;  
  fd         = argc > 1 ? open(argv[1], O_RDONLY) : STDIN_FILENO;
  if(fd < 0){
    printf("Open failed\n");
    exit(1);
  }
  nread      = read(fd, memory, MEM_SIZE*sizeof(long));
  if(nread < 0){
    printf("Read failed\n");
    exit(1);
  }
  nread /= 4;
  prog_end   = heap_base = hp = memory + nread;
  heap_size  = (MEM_SIZE - STACK_SIZE - nread)/2;
  upper_heap = hp + heap_size;

  /* sweep through the program image converting
     everything to host byte order */
  for(; nread > 0 ; nread--){
    memory[nread-1] = ntohl(memory[nread-1]);
  }

  NEXT;

  /* stack manipulation */
  INSTRUCTION(PUSH, STACK_PUSH(*pc++));
  INSTRUCTION(POP, STACK_POP());
  INSTRUCTION(SWAP,
	      do{
		long tmp = STACK(0);
		STACK(0) = STACK(1);
		STACK(1) = tmp;
	      }while(0)
	      );
  INSTRUCTION(DUP, STACK_PUSH(STACK(0)));
  INSTRUCTION(ROT,
	      do{
		long tmp = STACK(0);		
		STACK(0) = STACK(1);
		STACK(1) = STACK(2);
		STACK(2) = tmp;
	      }while(0)
	      );

  /* flow control */
  INSTRUCTION(CALL,
	      do{
		long tmp = pc - memory;
		ASSERT_TYPE(STACK(0), PTR);
		pc = memory + PTR_TARGET(STACK(0));
		STACK(0) = MAKE_PTR(tmp,0);
	      }while(0)
	      );

  INSTRUCTION(RET,
	      do{
		ASSERT_TYPE(STACK(1), PTR);
		pc = memory + PTR_TARGET(STACK(1));
		STACK(1) =  STACK(0);
		STACK_POP();
	      }while(0)
	      );

  INSTRUCTION(JMP, ASSERT_TYPE(STACK(0), PTR); pc = memory + PTR_TARGET(STACK_POP()));

  INSTRUCTION(JTRUE,  
	      do{
		ASSERT_TYPE(STACK(0), PTR);
		if(unlikely(!IS_BOOL(STACK(1)))){TYPE_ERROR(BOOL);}
		pc = (STACK(1) ==  TRUE_VAL) ? memory + PTR_TARGET(STACK(0)) : pc; 
		STACK_POP(); 
		STACK_POP();
	      }while(0)
	      );

  INSTRUCTION(END, 
#ifdef __STATS__
	      fprintf(stderr, "max_alloc = %d\n", max_alloc);
#endif
	      return 0);

  /* arithmetic */
  INSTRUCTION(PLUS, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) +  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());
    
  INSTRUCTION(MUL,
	      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) * NUM_TO_NATIVE(STACK(0))); 
	      STACK_POP());

  INSTRUCTION(SUB, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) -  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

  INSTRUCTION(DIV, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) / NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

  INSTRUCTION(MOD, 
	      ASSERT_TYPE(STACK(0), NUM);
	      ASSERT_TYPE(STACK(1), NUM);
	      STACK(1) = MAKE_NUM(NUM_TO_NATIVE(STACK(1)) %  NUM_TO_NATIVE(STACK(0)));
	      STACK_POP());

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
  INSTRUCTION(EQ, do{
      STACK(1) = (STACK(0) == STACK(1) ? TRUE_VAL : FALSE_VAL); 
      STACK_POP();
    }while(0));

  INSTRUCTION(LT, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_TYPE(STACK(1), NUM); 
      STACK(1) = (STACK(0) < STACK(1) ? TRUE_VAL : FALSE_VAL); 
      STACK_POP();
    }while(0));

  /* memory access */
  INSTRUCTION(STOR, do{
      ASSERT_TYPE(STACK(0), NUM)
      ASSERT_TYPE(STACK(1), PTR);
      if(NUM_TO_NATIVE(STACK(0)) < 0 || NUM_TO_NATIVE(STACK(0)) >= PTR_SIZE(STACK(1))){
	fprintf(stderr, "Invalid store: offset %d out of bounds\n", NUM_TO_NATIVE(STACK(0)));
	exit(1);
      }
      memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))] = STACK(2);
      STACK_POP();
      STACK_POP();
      STACK_POP();
    }while(0)
    );

  INSTRUCTION(ALOC, do{
      long __tmp;
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

  INSTRUCTION(LOAD, do{
      ASSERT_TYPE(STACK(0), NUM);
      ASSERT_TYPE(STACK(1), PTR);
      if(NUM_TO_NATIVE(STACK(0)) < 0 || NUM_TO_NATIVE(STACK(0)) > PTR_SIZE(STACK(1))){
	fprintf(stderr, "Invalid load: offset %d out of bounds\n", NUM_TO_NATIVE(STACK(0)));
	exit(1);
      }
      STACK(1) = memory[PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0))];
      STACK_POP();
    }while(0));
  
  /* I/O */
  INSTRUCTION(GETC, STACK_PUSH(MAKE_CHAR(getchar())));
  INSTRUCTION(DUMP, DO_DUMP(stdout));
  INSTRUCTION(PINT, ASSERT_TYPE(STACK(0), NUM);    printf("%ld", NUM_TO_NATIVE(STACK(0))); STACK_POP());
  INSTRUCTION(PCHR, ASSERT_TYPE(STACK(0), VCONST); putchar(CHAR_TO_NATIVE(STACK_POP())));

  /* Root register manipulation */
  INSTRUCTION(RDRR, STACK_PUSH(rr));
  INSTRUCTION(WTRR, rr = STACK_POP());

  /* type checking */
  INSTRUCTION(ISNUM,    STACK(0) = (IS_NUM(STACK(0)))    ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISLCONST, STACK(0) = (IS_LCONST(STACK(0))) ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISPTR,    STACK(0) = (IS_PTR(STACK(0)))    ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISBOOL,   STACK(0) = (IS_BOOL(STACK(0)))   ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISCHR,    STACK(0) = (IS_CHAR(STACK(0)))   ? TRUE_VAL : FALSE_VAL);
  INSTRUCTION(ISINS,    STACK(0) = (IS_INS(STACK(0)))    ? TRUE_VAL : FALSE_VAL);
  return 0;
}
  
