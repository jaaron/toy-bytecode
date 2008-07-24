#define MEM_SIZE   8192
#define STACK_SIZE 256

#define NUM    0x0
#define LCONST 0x1
#define VCONST 0x2
#define PTR    0x3

#define CHAR_FLAG 0x00800000
#define MAKE_VCONST(x) (((x) << 2) | VCONST)
#define MAKE_CHAR(x)   (((x) << 24) | CHAR_FLAG | VCONST)
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
#define I_SUB   INS(12)
#define I_DIV   INS(13)
#define I_MOD   INS(14)
#define I_SHL   INS(15)
#define I_SHR   INS(16)
#define I_BOR   INS(17)
#define I_BAND  INS(18)

/* Comparison */
/* EQ is applicable to all types,  
   LT and GT cause type errors if both STACK(0) and STACK(1) are
   non numeric */
#define I_EQ    INS(19)
#define I_LT    INS(20)

/* Reading and writing memory */
#define I_STOR  INS(21)  /* target address is top, then value */
#define I_LOAD  INS(22)
#define I_ALOC  INS(23)

/* I/O operations */
#define I_GETC  INS(24)
#define I_DUMP  INS(25)
#define I_PINT  INS(26)
#define I_PCHR  INS(27)

/* Root register manipulation */
#define I_RDRR  INS(28)
#define I_WTRR  INS(29)

/* Type checking instructions */
#define I_ISNUM    INS(30)
#define I_ISLCONST INS(31)
#define I_ISPTR    INS(32)
#define I_ISBOOL   INS(33)
#define I_ISCHR    INS(34)
#define I_ISINS    INS(35)

#define NR_INS 36

/* End instructions */

#define TRUE_VAL  MAKE_VCONST(128)
#define FALSE_VAL MAKE_VCONST(129)
