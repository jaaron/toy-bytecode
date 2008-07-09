/* macros for converting between 
   vm and native formats */
#define NATIVE_TO_INT(x)    (htonl(x << 2))
#define INT_TO_NATIVE(x)    (ntohl(x) >> 2)

#define NATIVE_TO_PTR(x,sz) ((htonl(((x) & 0x000fffff) | ((sz & 0xff) << 22) ) << 2) | 0x3)
#define PTR_TO_NATIVE(x)    (ntohl(x >> 2) & 0x000fffff)
#define PTR_SIZE(x)         (((unsigned long)ntohl(x >>2)) >> 22)

#define INT_TO_VM_CONST(x)     (x | 0x2)
#define INT_TO_LANG_CONST(x)   (x | 0x01)
#define INT_TO_PTR(x,sz)       NATIVE_TO_PTR(INT_TO_NATIVE(x),sz)
#define CONST_TO_INT           (x & 0xfffffffc)

#define INS(i)   ({long __tmp =(i&0x3fff); ((__tmp<<2)|2) | (htonl(__tmp<<2|2));})

#define MEM_SIZE 4096

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
#define I_JZ    INS(8)
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

/* I/O operations */
#define I_GETC  INS(18)
#define I_DUMP  INS(19)
#define I_PINT  INS(20)
#define I_PCHR  INS(21)
#define I_PSTR  INS(22)
/* End instructions */
