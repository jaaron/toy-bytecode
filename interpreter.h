#define MEM_SIZE   4096
#define STACK_SIZE 256

#define NUM    0x0
#define LCONST 0x1
#define VCONST 0x2
#define PTR    0x3

#define MAKE_VCONST(x) (((x) << 2) | VCONST)
#define MAKE_CHAR(x)   (((x) << 24) | VCONST)
#define MAKE_NUM(x)    ((x) << 2)
#define MAKE_PTR(x,sz) (((x) << 10) | (((sz)  & 0xff) << 2) | PTR)

#define INS(i) MAKE_VCONST(i)

/* Byte code instructions */
/* Basic stack manipulation */
#define I_PUSH  INS(0)
#define I_POP   INS(1)
#define I_SWAP  INS(2)
#define I_DUP   INS(3)
#define I_ROT   INS(4)

#define I_CALL  INS(5)
#define I_RET   INS(6)
/* jmp and jtrue are absolute jumps */
#define I_JMP   INS(7)
#define I_JTRUE INS(8)
#define I_END   INS(9)

/* Arithmetic */
#define I_ADD   INS(10)
#define I_MUL   INS(11)
#define I_SHL   INS(12)
#define I_SHR   INS(13)
#define I_BOR   INS(14)
#define I_BAND  INS(15)

/* Comparison */
/* EQ is applicable to all types,  
   LT and GT cause type errors if both STACK(0) and STACK(1) are
   non numeric */
#define I_EQ    INS(16)
#define I_LT    INS(17)

/* Reading and writing memory */
#define I_STOR  INS(18)  /* target address is top, then value */
#define I_LOAD  INS(19)
#define I_ALOC  INS(20)

/* I/O operations */
#define I_GETC  INS(21)
#define I_DUMP  INS(22)
#define I_PINT  INS(23)
#define I_PCHR  INS(24)

/* Root register manipulation */
#define I_RDRR  INS(25)
#define I_WTRR  INS(26)

/* Type checking instructions */
#define I_ISNUM    INS(27)
#define I_ISLCONST INS(28)
#define I_ISPTR    INS(29)
#define I_ISBOOL   INS(30)
#define I_ISCHR    INS(31)
#define I_ISINS    INS(32)

#define NR_INS 33

/* End instructions */

#define TRUE_VAL  MAKE_VCONST(128)
#define FALSE_VAL MAKE_VCONST(129)
