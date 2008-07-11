%{
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include "interpreter.h"

#define PACK_NUM(x)        (htonl((x) << 2))
#define PACK_PTR(x,sz)     (htonl(((x) << 10) | (((sz)&0xff) << 2) | 3))
#define PACK_VM_CONST(x)   (htonl(((x) << 2)  | 2))
#define PACK_LANG_CONST(x) (htonl(((x) << 2)  | 1))

  static struct {    //  1024 labels should be sufficient since
    char *txt;      //  a program is at the moment at most 1024 
    long dst;       //  words
  } labels[MEM_SIZE]; 
  static int nr_labels =0;

  static struct {
     char *txt;
     long dst;
  } unresolved_labels[MEM_SIZE];
  static long nr_unresolved = 0;

  static long out_buffer[MEM_SIZE];
  static long words = 0;

  static inline long decode_ptr(char *text, int base1, int base2){
    char *comma;
    unsigned long addr = strtoul(text, &comma, base1);    
    char sz = (*comma == '\0') ? 0xff : (char) (strtoul(comma+1, NULL, base2) & 0xff);
    return PACK_PTR(addr,sz);
  }
%}

%option noyywrap

DIGIT   [0-9]
XDIGIT  {DIGIT}|[a-fA-F]
D_NUM   (\+|-)?{DIGIT}+
X_NUM   0[xX]{XDIGIT}+
IDENT   [a-zA-Z_][a-zA-Z_0-9]*

%%

n{X_NUM} {out_buffer[words++] = PACK_NUM(strtoul(yytext+1, NULL, 16));}
n{D_NUM} {out_buffer[words++] = PACK_NUM(strtol(yytext+1, NULL, 10));}

p{X_NUM},{X_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 16, 16);}
p{X_NUM},{D_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 16, 10);}
p{D_NUM},{D_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 10, 10);}
p{D_NUM},{X_NUM} {out_buffer[words++] = decode_ptr(yytext+1, 10, 16);}

v{X_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtoul(yytext+1, NULL, 16));}
v{D_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtoul(yytext+1, NULL, 16));}
l{X_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtol(yytext+1, NULL, 10));}
l{D_NUM}         {out_buffer[words++] = PACK_VM_CONST(strtol(yytext+1, NULL, 10));}

;;.*\n         {}
#.*\n          {}
PUSH  {out_buffer[words++] = PACK_VM_CONST(I_PUSH);}
POP   {out_buffer[words++] = PACK_VM_CONST(I_POP); }
SWAP  {out_buffer[words++] = PACK_VM_CONST(I_SWAP);}
DUP   {out_buffer[words++] = PACK_VM_CONST(I_DUP); }
ROT   {out_buffer[words++] = PACK_VM_CONST(I_ROT); }
CALL  {out_buffer[words++] = PACK_VM_CONST(I_CALL);}
RET   {out_buffer[words++] = PACK_VM_CONST(I_RET); }
JMP   {out_buffer[words++] = PACK_VM_CONST(I_JMP); }
JZ    {out_buffer[words++] = PACK_VM_CONST(I_JZ);  }
END   {out_buffer[words++] = PACK_VM_CONST(I_END); }
ADD   {out_buffer[words++] = PACK_VM_CONST(I_ADD); }
MUL   {out_buffer[words++] = PACK_VM_CONST(I_MUL); }
SHL   {out_buffer[words++] = PACK_VM_CONST(I_SHL); }
SHR   {out_buffer[words++] = PACK_VM_CONST(I_SHR); }
BOR   {out_buffer[words++] = PACK_VM_CONST(I_BOR); }
BAND  {out_buffer[words++] = PACK_VM_CONST(I_BAND);}
STOR  {out_buffer[words++] = PACK_VM_CONST(I_STOR);}
LOAD  {out_buffer[words++] = PACK_VM_CONST(I_LOAD);}
GETC  {out_buffer[words++] = PACK_VM_CONST(I_GETC);}
DUMP  {out_buffer[words++] = PACK_VM_CONST(I_DUMP);}
PINT  {out_buffer[words++] = PACK_VM_CONST(I_PINT);}
PCHR  {out_buffer[words++] = PACK_VM_CONST(I_PCHR);}

:{IDENT} {
  int i;
  labels[nr_labels].txt = strdup(yytext+1);
  labels[nr_labels].dst = words;
  printf("label %s at offset %d\n", yytext+1, words);

  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt &&
       !strcmp(labels[nr_labels].txt, unresolved_labels[i].txt+1)){
      out_buffer[unresolved_labels[i].dst] = PACK_PTR(words,0xff);
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
    out_buffer[words] = PACK_PTR(labels[i].dst,0xff);
  }else{
    unresolved_labels[nr_unresolved].txt = strdup(yytext);
    unresolved_labels[nr_unresolved].dst = words;
    nr_unresolved++;
    out_buffer[words] = 0xffffffff; /* we'll fix this later on! */
  }
  words++;
}

[[:space:]]*   {}
<<EOF>>        { yyterminate(); }

.              { fprintf(stderr, "Error: unexpected token: %s after %d words\n", yytext, words); exit(1);}

%%

int main(int argc, char *argv[])
{
  int failure = 0;
  int i;
  int output_filedes = argc > 1 ? open(argv[1], O_WRONLY | O_CREAT) : STDOUT_FILENO;
  if(output_filedes < 0){
    printf("Failed to open output file.  Dying\n");
    exit(1);
  }
  yylex();
  if(words > 1024){
    fprintf(stderr, "Warning: Program is more than 1024 words.  "
	    "Interpreter may fail\n");
    failure = 1;
  }
  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt != NULL){
      fprintf(stderr, "Unresolved reference to label: \"%s\" at offset %d\n",
	      unresolved_labels[i].txt, unresolved_labels[i].dst);
      failure = 1;
    }
  }
  if(failure){
    fprintf(stderr, "Dying From Failure\n");
    exit(1);
  }
  write(output_filedes, out_buffer, words*sizeof(long));
  fprintf(stderr, "Wrote %d words\n", words);
  close(output_filedes);
  return 0;
}


