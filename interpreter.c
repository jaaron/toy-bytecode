#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include "interpreter.h"

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

#define MEM_SIZE 4096

#define DO_DUMP(stream) do{					\
    int q=0;							\
    fprintf(stream, "pc: %d, sp: %p height: %d\nstack:\n",	\
	    pc-memory, sp, STACK_HEIGHT());			\
    while(q < STACK_HEIGHT() ){					\
      switch(CELL_TYPE(STACK(q))){				\
      case NUM:							\
	fprintf(stream, "\t%d\n", NUM_TO_NATIVE(STACK(q)));	\
	break;							\
      case VCONST:						\
	fprintf(stream, "\tvc: %d\n", NUM_TO_NATIVE(STACK(q)));	\
	break;							\
      case LCONST:						\
	fprintf(stream, "\tlc: %d\n", NUM_TO_NATIVE(STACK(q)));	\
	break;							\
      case PTR:							\
	fprintf(stream, "\tptr: %d sz: %d\n",			\
		PTR_TARGET(STACK(q)), PTR_SIZE(STACK(q)));	\
	break;							\
      }								\
      q++;							\
    }								\
  }while(0)

#define IDX_FROM_INS(i) (i >> 2)

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

#define NUM    0x0
#define LCONST 0x1
#define VCONST 0x2
#define PTR    0x3

#define unlikely(x)     __builtin_expect((x),0)
#define likely(x)     __builtin_expect((x),1)

#define MAKE_PTR(x,sz) ( (x) << 10 | ((sz)  & 0xff) << 2 | PTR)
#define PTR_TARGET(x) (((unsigned long)x) >> 10)
#define PTR_SIZE(x)   ((((unsigned long)x) >> 2) & 0xff)

#define NUM_TO_NATIVE(x) ((x) >> 2 | NUM)
#define MAKE_NUM(x) ((x) << 2)

#define MAKE_CHAR(x) (((x) << 24) | VCONST)
#define CHAR_TO_NATIVE(x) ((x) >> 24)

// MAKE_TYPE doesn't work for pointers!
#define MAKE_TYPE(x, t) ((x << 2) | t)

#define CELL_TYPE(x) ((x) & 0x3)

#define IS_NUM(x) (CELL_TYPE(x) == NUM)
#define IS_PTR(x) (CELL_TYPE(x) == PTR)
#define IS_CONST(x) ({char __tmp = CELL_TYPE(x); (x == LCONST) || (x == VCONST)})

#define ASSERT_TYPE(x,t)     if(unlikely(CELL_TYPE(x) != t)){TYPE_ERROR(t);}
#define ASSERT_NOT_TYPE(x,t) if(unlikely(CELL_TYPE(x) == t)){TYPE_ERROR(!t);}

#define HEAP_CHECK(n) do{				\
  long __tmp = n;					\
  if(hp + __tmp > sp){					\
    fprintf(stderr, "Unable to allocate memory!\n");	\
    exit(-ENOMEM);					\
  }							\
  }while(0);

#define TYPE_ERROR(t) do{			\
    fprintf(stderr, "Type error. Expected "#t);	\
    DO_DUMP(stderr);				\
    exit(-1);					\
  }while(0);

int main(int argc, char *argv[])
{
  static long instructions[] = {
    (long)&&PUSH, (long)&&POP,
    (long)&&SWAP, (long)&&DUP, 
    (long)&&ROT,

    /* Control flow */
    (long)&&CALL,
    (long)&&RET,  (long)&&JMP,  
    (long)&&JEQ,    (long)&&END,
    
    /* Arithmetic */
    (long)&&PLUS, (long)&&MUL, 
    (long)&&SHL,  (long)&&SHR, 
    (long)&&BOR,  (long)&&BAND,
    
    /* Reading and writing memory */
    (long)&&STOR, (long)&&LOAD, (long)&&ALOC,
    
    /* I/0 */
    (long)&&GETC, (long)&&DUMP, 
    (long)&&PINT, (long)&&PCHR, 
  };

  int fd;
  long memory1[MEM_SIZE], memory2[MEM_SIZE], *memory = memory1;
  long *sp = &memory[MEM_SIZE-1];
  long *pc = memory;
  long *hp;
  long  rr = MAKE_PTR(0,0); /* the root regiseter can be used by the language
			       to store an additional root for gc (anything 
			       on the stack is also treated as a root). */
  long nread;  
  fd    = argc > 1 ? open(argv[1], O_RDONLY) : STDIN_FILENO;
  nread = read(fd, memory, MEM_SIZE*sizeof(long))/sizeof(long);  

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

  INSTRUCTION(JEQ,  
	      do{
		ASSERT_TYPE(STACK(0), PTR);
		pc = STACK(1) ==  STACK(2) ? memory + PTR_TARGET(STACK(0)) : pc; 
		STACK_POP(); 
		STACK_POP();
		STACK_POP();
	      }while(0)
	      );
  INSTRUCTION(END, return 0);

  /* arithmetic */
  INSTRUCTION(PLUS,
	      ASSERT_TYPE(STACK(0), NUM);
	      switch(CELL_TYPE(STACK(1))){
		case VCONST:
		case LCONST:
		case NUM:
		  STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) +  NUM_TO_NATIVE(STACK(0)), CELL_TYPE(STACK(1)));
		  break;
		case PTR:
		  if(NUM_TO_NATIVE(STACK(0)) < 0){
		    fprintf(stderr, "Invalid pointer arithmetic: negative offset %d is unallowed\n",
			    NUM_TO_NATIVE(STACK(0)));
		    DO_DUMP(stderr);
		    exit(1);
		  }
		  if(PTR_SIZE(STACK(1)) > NUM_TO_NATIVE(STACK(0)) || 
		     PTR_SIZE(STACK(1) == 0xff)){
		    STACK(1) = MAKE_PTR(PTR_TARGET(STACK(1)) + NUM_TO_NATIVE(STACK(0)),
					(PTR_SIZE(STACK(1)) == 0xff ? 
					 0xff : PTR_SIZE(STACK(1)) - NUM_TO_NATIVE(STACK(0))));
		  }else{
		    fprintf(stderr, "Invalid pointer arithmetic: offset %d is greater than size %d\n",
			    NUM_TO_NATIVE(STACK(0)), PTR_SIZE(STACK(1)));
		    DO_DUMP(stderr);
		    exit(1);
		  }
		  break;
		}
		STACK_POP();
	      );
    
  INSTRUCTION(MUL, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_NOT_TYPE(STACK(1), PTR);
      STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) * NUM_TO_NATIVE(STACK(0)),CELL_TYPE(STACK(1))); 
      STACK_POP();
    }while(0));

  INSTRUCTION(SHL, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_NOT_TYPE(STACK(1), PTR);
      STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) << NUM_TO_NATIVE(STACK(0)),CELL_TYPE(STACK(1))); 
      STACK_POP();
    }while(0));

  INSTRUCTION(SHR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_NOT_TYPE(STACK(1), PTR);
      STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) >> NUM_TO_NATIVE(STACK(0)),CELL_TYPE(STACK(1))); 
      STACK_POP();
    }while(0));

  INSTRUCTION(BOR, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_NOT_TYPE(STACK(1), PTR);
      STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) | NUM_TO_NATIVE(STACK(0)),CELL_TYPE(STACK(1))); 
      STACK_POP();
    }while(0));

  INSTRUCTION(BAND, do{
      ASSERT_TYPE(STACK(0), NUM); ASSERT_NOT_TYPE(STACK(1), PTR);
      STACK(1) = MAKE_TYPE(NUM_TO_NATIVE(STACK(1)) & NUM_TO_NATIVE(STACK(0)),CELL_TYPE(STACK(1))); 
      STACK_POP();
    }while(0));
  
  /* memory access */
  INSTRUCTION(STOR, do{
      ASSERT_TYPE(STACK(0), PTR);
      memory[PTR_TARGET(STACK(0))] = STACK(1);
      STACK_POP();
      STACK_POP();
    }while(0)
    );

  INSTRUCTION(ALOC, do{
      long __tmp;
      ASSERT_TYPE(STACK(0), NUM);
      HEAP_CHECK(NUM_TO_NATIVE(STACK(0)));
      __tmp = MAKE_PTR(hp - memory, STACK(0));
      hp += NUM_TO_NATIVE(STACK(0));
      STACK(0) = __tmp;
    }while(0));

  INSTRUCTION(LOAD, ASSERT_TYPE(STACK(0), PTR); STACK(0) = memory[PTR_TARGET(STACK(0))]);

  /* I/O */
  INSTRUCTION(GETC, STACK_PUSH(MAKE_CHAR(getchar())));
  INSTRUCTION(DUMP, DO_DUMP(stdout));
  INSTRUCTION(PINT, ASSERT_TYPE(STACK(0), NUM);    printf("%ld\n", NUM_TO_NATIVE(STACK(0))); STACK_POP());
  INSTRUCTION(PCHR, ASSERT_TYPE(STACK(0), VCONST); putchar(CHAR_TO_NATIVE(STACK_POP())));
  return 0;
}
  
