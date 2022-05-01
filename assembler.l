/* Copyright 2012 J. Aaron Pendergrass						*/
/*										*/
/* This file is part of toy-bytecode.						*/
/*										*/
/* toy-bytecode is free software: you can redistribute it and/or modify		*/
/* it under the terms of the GNU General Public License as published by		*/
/* the Free Software Foundation, either version 3 of the License, or		*/
/* (at your option) any later version.						*/
/*										*/
/* toy-bytecode is distributed in the hope that it will be useful,		*/
/* but WITHOUT ANY WARRANTY; without even the implied warranty of		*/
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		*/
/* GNU General Public License for more details.					*/
/*										*/
/* You should have received a copy of the GNU General Public License		*/
/* along with toy-bytecode.  If not, see <http://www.gnu.org/licenses/>.	*/

%{
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include "interpreter.h"

#define PACK_NUM(x)        htonl(MAKE_NUM(x))
#define PACK_PTR(x,sz)     htonl(MAKE_PTR(x,sz))
#define PACK_VM_CONST(x)   htonl(x)
#define PACK_LANG_CONST(x) htonl(MAKE_LCONST(x))
#define PACK_CHAR(x)       PACK_VM_CONST(MAKE_CHAR(x))

  static struct {    //  1024 labels should be sufficient since
    char *txt;      //  a program is at the moment at most 1024 
    word dst;       //  words
  } labels[MEM_SIZE]; 
  static int nr_labels =0;

  static struct {
      char *txt;
      word dst;
  } unresolved_labels[MEM_SIZE];
  static word nr_unresolved = 0;

  static word out_buffer[MEM_SIZE];
  static word words = 0;
  static word __line_number=0;
  static inline word decode_ptr(char *text, int base1, int base2){
    char *comma;
    unsigned long addr = strtoul(text, &comma, base1);    
    char sz = (*comma == '\0') ? 0 : (char) (strtoul(comma+1, NULL, base2) & PTR_SIZE_MASK);
    return PACK_PTR(addr,sz);
  }
%}

%option noyywrap

DIGIT   [0-9]
XDIGIT  {DIGIT}|[a-fA-F]
D_NUM   (\+|-)?{DIGIT}+
X_NUM   0[xX]{XDIGIT}+
IDENT   [a-zA-Z_][a-zA-Z_0-9]*

STRCHARS (\\\")|(\\\\)|(\\\')|(\\a)|(\\b)|(\\f)|(\\n)|(\\r)|(\\t)|(\\v)|([^\"\\])

%%
'[^\\]'   {out_buffer[words++] = PACK_CHAR(*(yytext+1));}
'\\\\'    {out_buffer[words++] = PACK_CHAR('\\');}
'\\\''    {out_buffer[words++] = PACK_CHAR('\'');}
'\\\"'    {out_buffer[words++] = PACK_CHAR('"');}
'\\a'     {out_buffer[words++] = PACK_CHAR('\a');}
'\\b'     {out_buffer[words++] = PACK_CHAR('\b');}
'\\f'     {out_buffer[words++] = PACK_CHAR('\f');}
'\\n'     {out_buffer[words++] = PACK_CHAR('\n');}
'\\r'     {out_buffer[words++] = PACK_CHAR('\r');}
'\\t'     {out_buffer[words++] = PACK_CHAR('\t');}
'\\v'     {out_buffer[words++] = PACK_CHAR('\v');}
'\\x{XDIGIT}{XDIGIT}' {out_buffer[words++] = PACK_CHAR(strtoul(yytext+3, NULL, 16));}
"\""{STRCHARS}*"\"" {
  int i;
  for(i=1;i<yyleng-1;i++){
    if(yytext[i] == '\\'){
      i++;
      switch(yytext[i]){
      case '\\': out_buffer[words++] = PACK_CHAR('\\');
	break;
      case '\'': out_buffer[words++] = PACK_CHAR('\'');
	break;
      case '\"': out_buffer[words++] = PACK_CHAR('\"');
	break;
      case 'a': out_buffer[words++] = PACK_CHAR('\a');
	break;
      case 'b': out_buffer[words++] = PACK_CHAR('\b');
	break;
      case 'f': out_buffer[words++] = PACK_CHAR('\f');
	break;
      case 'n': out_buffer[words++] = PACK_CHAR('\n');
	break;
      case 'r': out_buffer[words++] = PACK_CHAR('\r');
	break;
      case 't': out_buffer[words++] = PACK_CHAR('\t');
	break;
      case 'v': out_buffer[words++] = PACK_CHAR('\v');
	break;
      case 'x': {
	char tmp[3] = {yytext[i+1], yytext[i+2], '\0'};
	out_buffer[words++] = PACK_CHAR(strtoul(tmp, NULL, 16));
	i+=2;
	break;
      }
      default:
	fprintf(stderr, 
		"Unrecognized escape character '%c' in string constant: %s\n",
		yytext[i], yytext);
	exit(1);
      }
    }else{
      out_buffer[words++] = PACK_CHAR(yytext[i]);
    }
  }
}

n{X_NUM} {out_buffer[words++] = PACK_NUM(strtoul(yytext+1, NULL, 16));}
n{D_NUM} {out_buffer[words++] = PACK_NUM(strtol(yytext+1, NULL, 10));}

p{X_NUM},{X_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 16, 16);}
p{X_NUM},{D_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 16, 10);}
p{D_NUM},{D_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 10, 10);}
p{D_NUM},{X_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 10, 16);}

v{X_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtoul(yytext+1, NULL, 16));}
v{D_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtoul(yytext+1, NULL, 10));}
l{X_NUM}         {out_buffer[words++] = PACK_LANG_CONST(strtol(yytext+1, NULL, 16));}
l{D_NUM}         {out_buffer[words++] = PACK_LANG_CONST(strtol(yytext+1, NULL, 10));}

;;.*\n         {__line_number++;}
#.*\n          {__line_number++;}
PUSH     {out_buffer[words++] = PACK_VM_CONST(I_PUSH);}
POP      {out_buffer[words++] = PACK_VM_CONST(I_POP); }
SWAP     {out_buffer[words++] = PACK_VM_CONST(I_SWAP);}
DUP      {out_buffer[words++] = PACK_VM_CONST(I_DUP); }
ROT      {out_buffer[words++] = PACK_VM_CONST(I_ROT); }
CALL     {out_buffer[words++] = PACK_VM_CONST(I_CALL);}
RET      {out_buffer[words++] = PACK_VM_CONST(I_RET); }
JMP      {out_buffer[words++] = PACK_VM_CONST(I_JMP); }
JTRUE    {out_buffer[words++] = PACK_VM_CONST(I_JTRUE); }
END      {out_buffer[words++] = PACK_VM_CONST(I_END); }
ADD      {out_buffer[words++] = PACK_VM_CONST(I_ADD); }
MUL      {out_buffer[words++] = PACK_VM_CONST(I_MUL); }
SUB      {out_buffer[words++] = PACK_VM_CONST(I_SUB); }
DIV      {out_buffer[words++] = PACK_VM_CONST(I_DIV); }
MOD      {out_buffer[words++] = PACK_VM_CONST(I_MOD); }
SHL      {out_buffer[words++] = PACK_VM_CONST(I_SHL); }
SHR      {out_buffer[words++] = PACK_VM_CONST(I_SHR); }
BOR      {out_buffer[words++] = PACK_VM_CONST(I_BOR); }
BAND     {out_buffer[words++] = PACK_VM_CONST(I_BAND);}
EQ       {out_buffer[words++] = PACK_VM_CONST(I_EQ);  }
LT       {out_buffer[words++] = PACK_VM_CONST(I_LT);  }
STOR     {out_buffer[words++] = PACK_VM_CONST(I_STOR);}
LOAD     {out_buffer[words++] = PACK_VM_CONST(I_LOAD);}
ALOC     {out_buffer[words++] = PACK_VM_CONST(I_ALOC);}
GETC     {out_buffer[words++] = PACK_VM_CONST(I_GETC);}
DUMP     {out_buffer[words++] = PACK_VM_CONST(I_DUMP);}
PINT     {out_buffer[words++] = PACK_VM_CONST(I_PINT);}
PCHR     {out_buffer[words++] = PACK_VM_CONST(I_PCHR);}
RDRR     {out_buffer[words++] = PACK_VM_CONST(I_RDRR);}
WTRR     {out_buffer[words++] = PACK_VM_CONST(I_WTRR);}
ISNUM    {out_buffer[words++] = PACK_VM_CONST(I_ISNUM);}
ISPTR    {out_buffer[words++] = PACK_VM_CONST(I_ISPTR);}
ISLCONST {out_buffer[words++] = PACK_VM_CONST(I_ISLCONST);}
ISCHR    {out_buffer[words++] = PACK_VM_CONST(I_ISCHR);}
ISBOOL   {out_buffer[words++] = PACK_VM_CONST(I_ISBOOL);}
ISINS    {out_buffer[words++] = PACK_VM_CONST(I_ISINS);}

PBIN       {out_buffer[words++] = PACK_VM_CONST(I_PBIN);}
PBLCONSTI  {out_buffer[words++] = PACK_VM_CONST(I_PBLCONSTI);}
PBVCONSTI {out_buffer[words++] = PACK_VM_CONST(I_PBVCONSTI);}
PBPTRI     {out_buffer[words++] = PACK_VM_CONST(I_PBPTRI);}

TRUE     {out_buffer[words++] = PACK_VM_CONST(TRUE_VAL);}
FALSE    {out_buffer[words++] = PACK_VM_CONST(FALSE_VAL);}
EOF      {out_buffer[words++] = PACK_CHAR(EOF);}

:{IDENT},{D_NUM} {
  int i;
  char *comma     = strchr(yytext,',');
  word sz      = atoi(comma+1);
  labels[nr_labels].txt = malloc(sizeof(char)*(comma - yytext));
  memcpy(labels[nr_labels].txt, yytext+1, comma - yytext - 1);
  labels[nr_labels].txt[comma - yytext-1] = '\0';
  labels[nr_labels].dst = PACK_PTR(words, sz);
#ifdef __PRINT_LABELS__
 fprintf(stderr, "label %s at offset %u size: %u\n", yytext+1, words, sz);
#endif

  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt &&
       !strcmp(labels[nr_labels].txt, unresolved_labels[i].txt+1)){
      out_buffer[unresolved_labels[i].dst] = labels[nr_labels].dst;
      free(unresolved_labels[i].txt);
      unresolved_labels[i].txt = NULL;
    }
  }

  nr_labels++;
 }

:{IDENT} {
  int i;
  labels[nr_labels].txt = strdup(yytext+1);
  labels[nr_labels].dst = PACK_PTR(words,0);
#ifdef __PRINT_LABELS__
  fprintf(stderr, "label %s at offset %u unknown size\n", yytext+1, words);
#endif

  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt &&
       !strcmp(labels[nr_labels].txt, unresolved_labels[i].txt+1)){
      out_buffer[unresolved_labels[i].dst] = labels[nr_labels].dst;
      free(unresolved_labels[i].txt);
      unresolved_labels[i].txt = NULL;
    }
  }

  nr_labels++;
 }

@{IDENT} {
  int i = 0;
  while(i < nr_labels && strcmp(yytext+1, labels[i].txt)){
    i++;
  }
  if(i < nr_labels){
    out_buffer[words] = labels[i].dst;
  }else{
    unresolved_labels[nr_unresolved].txt = strdup(yytext);
    unresolved_labels[nr_unresolved].dst = words;
    nr_unresolved++;
    out_buffer[words] = 0xffffffff; /* we'll fix this later on! */
  }
  words++;
}

\n             {__line_number++;}
[[:space:]]*   {}
<<EOF>>        { yyterminate(); }
.              { fprintf(stderr, "Error: unexpected token: %s after %u words (line: %u)\n", yytext, words, __line_number+1); exit(1);}

%%

int main(int argc, char *argv[])
{
  int failure = 0;
  int i;
  int output_filedes = argc > 1 ? open(argv[1], O_WRONLY | O_CREAT, 0644) : STDOUT_FILENO;
  if(output_filedes < 0){
    fprintf(stderr, "Failed to open output file.  Dying\n");
    exit(1);
  }
  yylex();

  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt != NULL){
      fprintf(stderr, "Unresolved reference to label: \"%s\" at offset %u (line: %u)\n",
	      unresolved_labels[i].txt, unresolved_labels[i].dst, __line_number+1);
      failure = 1;
    }
  }
  if(failure){
    fprintf(stderr, "Dying From Failure\n");
    exit(1);
  }
  if(write(output_filedes, out_buffer, words*sizeof(word)) < words*sizeof(word)){
      fprintf(stderr, "Error while writing!\n");
      close(output_filedes);
      return 1;
  }
  fprintf(stderr, "Read %d lines. Wrote %u words\n", __line_number, words);
  close(output_filedes);
  return 0;
}
