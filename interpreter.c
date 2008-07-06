#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
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

#define DO_DUMP(stream) do{						\
  int q=0;								\
  fprintf(stream, "pc: %d, sp: %p height: %d\nstack:\n",		\
	  pc-memory, sp, STACK_HEIGHT());				\
  while(q < STACK_HEIGHT() ){						\
    fprintf(stream, "\t%ld\n", STACK(q++));				\
  }									\
  }while(0)

#ifdef __TRACE__

#define NEXT do{						\
    fprintf(stderr,"==================================\n");	\
    fprintf(stderr,"initial pc: %d instruction: %d ",		\
	    pc-memory, *pc);					\
    goto *instructions[ntohl(*pc++)];				\
  }while(0)

#define INSTRUCTION(n,x)			\
  n:						\
  fprintf(stderr, #n "\n");			\
  x;						\
  DO_DUMP(stderr);				\
  NEXT

#else /* quiet versions! */

#define NEXT goto *instructions[ntohl(*pc++)]
#define INSTRUCTION(n,x)			\
  n:						\
  x;						\
  NEXT
#endif

#define READ_CHAR(c) I_PUSH, -c, I_PLUS, I_JZ, 1, I_END

int main(int argc, char *argv[])
{
  static long instructions[] = {
    (long)&&PUSH, (long)&&POP,
    (long)&&SWAP, (long)&&DUP, 
    (long)&&ROT,

    /* Control flow */
    (long)&&CALL,
    (long)&&RET,  (long)&&JMP,  
    (long)&&JZ,    (long)&&END,
    
    /* Arithmetic */
    (long)&&PLUS, (long)&&MUL, 
    (long)&&SHL,  (long)&&SHR, 
    (long)&&BOR,  (long)&&BAND,
    
    /* Reading and writing memory */
    (long)&&STOR, (long)&&LOAD,
    
    /* I/0 */
    (long)&&GETC, (long)&&DUMP, 
    (long)&&PINT, (long)&&PCHR, 
    (long)&&PSTR
  };

  int fd;
  long memory[MEM_SIZE];
  long *sp = &memory[MEM_SIZE-1];
  long *pc = memory;

  fd = open(argv[1], O_RDONLY);  
  read(fd, memory, MEM_SIZE*sizeof(long));  

  NEXT;

  /* stack manipulation */
  INSTRUCTION(PUSH, STACK_PUSH(ntohl(*pc++)));
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
		pc = memory + STACK(0);
		STACK(0) = tmp;
	      }while(0)
	      );

  INSTRUCTION(RET,
	      do{
		pc = memory + STACK(1);
		STACK(1) =  STACK(0);
		STACK_POP();
	      }while(0)
	      );
  INSTRUCTION(JMP, pc = memory + STACK_POP());
  INSTRUCTION(JZ,  pc = STACK(1) ? pc : memory + STACK(0); STACK_POP() ; STACK_POP() );
  INSTRUCTION(END, return 0);

  /* arithmetic */
  INSTRUCTION(PLUS, STACK(1) +=  STACK(0); STACK_POP());
  INSTRUCTION(MUL,  STACK(1) *=  STACK(0); STACK_POP());
  INSTRUCTION(SHL,  STACK(1) <<= STACK(0); STACK_POP());
  INSTRUCTION(SHR,  STACK(1) >>= STACK(0); STACK_POP());
  INSTRUCTION(BOR,  STACK(1) |=  STACK(0); STACK_POP());
  INSTRUCTION(BAND, STACK(1) &=  STACK(0); STACK_POP());
  
  /* memory access */
  INSTRUCTION(STOR, do{
      memory[STACK(0)] = htonl(STACK(1));
      STACK_POP();
      STACK_POP();
    }while(0)
    );

  INSTRUCTION(LOAD, STACK(0) = ntohl(memory[STACK(0)]));

  /* I/O */
  INSTRUCTION(GETC, STACK_PUSH(getchar()));
  INSTRUCTION(DUMP, DO_DUMP(stdout));
  INSTRUCTION(PINT, printf("%ld\n", STACK_POP()));
  INSTRUCTION(PSTR, printf("%s\n", (char *)(&memory[STACK_POP()])));
  INSTRUCTION(PCHR, putchar(STACK_POP()));
  return 0;
}


    
    
