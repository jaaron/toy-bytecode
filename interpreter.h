#define MEM_SIZE   4096
#define STACK_SIZE 256

#define INS(i) (i)

/* Byte code instructions */
/* Basic stack manipulation */
#define I_PUSH  INS(0)
#define I_POP   INS(1)
#define I_SWAP  INS(2)
#define I_DUP   INS(3)
#define I_ROT   INS(4)

#define I_CALL  INS(5)
#define I_RET   INS(6)
/* jmp and jz are absolute jumps */
#define I_JMP   INS(7)
#define I_JEQ   INS(8)
#define I_END   INS(9)

/* Arithmetic */
#define I_ADD   INS(10)
#define I_MUL   INS(11)
#define I_SHL   INS(12)
#define I_SHR   INS(13)
#define I_BOR   INS(14)
#define I_BAND  INS(15)

/* Reading and writing memory */
#define I_STOR  INS(16)  /* target address is top, then value */
#define I_LOAD  INS(17)
#define I_ALOC  INS(18)

/* I/O operations */
#define I_GETC  INS(19)
#define I_DUMP  INS(20)
#define I_PINT  INS(21)
#define I_PCHR  INS(22)

/* Root register manipulation */
#define I_RDRR  INS(23)
#define I_WTRR  INS(24)
/* End instructions */
