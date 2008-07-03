#define MEM_SIZE 4096

/* Byte code instructions */
/* Basic stack manipulation */
#define I_PUSH 0
#define I_POP  1
#define I_SWAP 2
#define I_DUP  3
#define I_ROT  4

#define I_CALL  5
#define I_RET   6
/* jmp and jz are relative jumps */
#define I_JMP   7
#define I_JZ    8
#define I_END   9

/* Arithmetic */
#define I_ADD   10
#define I_MUL   11
#define I_SHL   12
#define I_SHR   13
#define I_BOR   14
#define I_BAND  15

/* Reading and writing memory */
#define I_STOR  16
#define I_LOAD  17

/* I/O operations */
#define I_GETC  18
#define I_DUMP  19
#define I_PINT  20
#define I_PCHR  21
#define I_PSTR  22
/* End instructions */
