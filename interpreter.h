#define MEM_SIZE 4096

/* Byte code instructions */
/* Basic stack manipulation */
#define I_PUSH 0
#define I_POP  1
#define I_SWAP 2
#define I_DUP  3


#define I_CALL  4
#define I_RET   5
/* jmp and jz are relative jumps */
#define I_JMP   6
#define I_JZ    7
#define I_END   8

/* Arithmetic */
#define I_ADD   9
#define I_MUL   10
#define I_SHL   11
#define I_SHR   12
#define I_BOR   13

/* Reading and writing memory */
#define I_STOR  14
#define I_LOAD  15

/* I/O operations */
#define I_GETC  16
#define I_DUMP  17
#define I_PINT  18
#define I_PCHR  19
#define I_PSTR  20
/* End instructions */
