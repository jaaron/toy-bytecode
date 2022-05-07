/* Copyright 2012 J. Aaron Pendergrass					*/
/*									*/
/* This file is part of Foobar.						*/
/*									*/
/* Foobar is free software: you can redistribute it and/or modify	*/
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation, either version 3 of the License, or	*/
/* (at your option) any later version.					*/
/*									*/
/* Foobar is distributed in the hope that it will be useful,		*/
/* but WITHOUT ANY WARRANTY; without even the implied warranty of	*/
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	*/
/* GNU General Public License for more details.				*/
/*									*/
/* You should have received a copy of the GNU General Public License	*/
/* along with Foobar.  If not, see <http://www.gnu.org/licenses/>.	*/

#define MEM_SIZE   (1<<PTR_TARGET_BITS)
#define STACK_SIZE 0x400

typedef int32_t word;
typedef uint32_t uword;
#define FLAG_BITS 2

#define DATA_BITS ((sizeof(word)<<3)-FLAG_BITS)
#define FLAG_MASK (((1<<FLAG_BITS)-1) << DATA_BITS)
#define DATA_MASK (~FLAG_MASK)

#define NUM    (0x3 << DATA_BITS)
#define LCONST (0x1 << DATA_BITS)
#define VCONST (0x0)
#define PTR    (0x2 << DATA_BITS)

#define CHAR_FLAG (0x00800000)

#define MAKE_VCONST(x) (x)
#define MAKE_LCONST(x) (((x) & ~NUM) | LCONST)
#define MAKE_CHAR(x)   (CHAR_FLAG | ((x) & 0x007fffff))
#define MAKE_NUM(x)    (((x) & ~NUM) | NUM)

#define PTR_TARGET_BITS  18
#define PTR_SIZE_MASK    ((1<<(DATA_BITS - PTR_TARGET_BITS))-1)
#define MAX_PTR_SIZE     PTR_SIZE_MASK
#define PTR_TARGET_MASK  ((1<<PTR_TARGET_BITS)-1)
#define MAKE_PTR(x,sz)   (PTR | ((((uword)sz) & PTR_SIZE_MASK) << PTR_TARGET_BITS) | ((x) & PTR_TARGET_MASK))

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

/* Binary I/O */
#define I_PBIN INS(36)
/* Binary I/O with type conversion from integer */
#define I_PBLCONSTI INS(37)
#define I_PBVCONSTI INS(38)
#define I_PBPTRI INS(39)

#define I_BRK INS(40)

#define NR_INS 41

/* End instructions */

#define TRUE_VAL  MAKE_VCONST(128)
#define FALSE_VAL MAKE_VCONST(129)
