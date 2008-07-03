%{
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include "interpreter.h"

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


%}

%option noyywrap

DIGIT   [0-9]
XDIGIT  {DIGIT}|[a-fA-F]
D_NUM   (\+|-)?{DIGIT}+
X_NUM   0[xX]{XDIGIT}+
IDENT   [a-zA-Z_][a-zA-Z_0-9]*

%%

{X_NUM} {out_buffer[words++] = htonl(strtol(yytext, NULL, 16));}
{D_NUM} {out_buffer[words++] = htonl(strtol(yytext, NULL, 10));}

;;.*\n         {}
#.*\n          {}
PUSH  {out_buffer[words++] = htonl(I_PUSH);}
POP   {out_buffer[words++] = htonl(I_POP); }
SWAP  {out_buffer[words++] = htonl(I_SWAP);}
DUP   {out_buffer[words++] = htonl(I_DUP); }
ROT   {out_buffer[words++] = htonl(I_ROT); }
CALL  {out_buffer[words++] = htonl(I_CALL);}
RET   {out_buffer[words++] = htonl(I_RET); }
JMP   {out_buffer[words++] = htonl(I_JMP); }
JZ    {out_buffer[words++] = htonl(I_JZ);  }
END   {out_buffer[words++] = htonl(I_END); }
ADD   {out_buffer[words++] = htonl(I_ADD); }
MUL   {out_buffer[words++] = htonl(I_MUL); }
SHL   {out_buffer[words++] = htonl(I_SHL); }
SHR   {out_buffer[words++] = htonl(I_SHR); }
BOR   {out_buffer[words++] = htonl(I_BOR); }
BAND  {out_buffer[words++] = htonl(I_BAND);}
STOR  {out_buffer[words++] = htonl(I_STOR);}
LOAD  {out_buffer[words++] = htonl(I_LOAD);}
GETC  {out_buffer[words++] = htonl(I_GETC);}
DUMP  {out_buffer[words++] = htonl(I_DUMP);}
PINT  {out_buffer[words++] = htonl(I_PINT);}
PCHR  {out_buffer[words++] = htonl(I_PCHR);}
PSTR  {out_buffer[words++] = htonl(I_PSTR);}

:{IDENT} {
  int i;
  labels[nr_labels].txt = strdup(yytext+1);
  labels[nr_labels].dst = words;
  printf("label %s at offset %d\n", yytext+1, words);

  for(i=0;i<nr_unresolved;i++){
    if(unresolved_labels[i].txt &&
       !strcmp(labels[nr_labels].txt, unresolved_labels[i].txt+1)){
      out_buffer[unresolved_labels[i].dst] = 
	htonl(unresolved_labels[i].txt[0] == '@' ? 
	      words : words - (unresolved_labels[i].dst+2));
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
    out_buffer[words] = htonl(labels[i].dst);
  }else{
    unresolved_labels[nr_unresolved].txt = strdup(yytext);
    unresolved_labels[nr_unresolved].dst = words;
    nr_unresolved++;
    out_buffer[words] = htonl(0xdeadbeef);
  }
  words++;
}

">"{IDENT}   {
  int i = 0;
  while(i < nr_labels && strcmp(yytext+1, labels[i].txt)){
    i++;
  }
  if(i < nr_labels){
    out_buffer[words] = htonl(labels[i].dst - (words + 2));
  }else{
    unresolved_labels[nr_unresolved].txt = strdup(yytext);
    unresolved_labels[nr_unresolved].dst = words;
    out_buffer[words] = htonl(0xdeadbeef);
    nr_unresolved++;
  }
  words++;
}

[[:space:]]*   {}
<<EOF>>        { yyterminate(); }

.              { fprintf(stderr, "Error: unexpected token: %s", yytext); exit(1);}

%%

int main(int argc, char *argv[])
{
  int failure = 0;
  int i;
  int output_filedes = argc > 1 ? open(argv[1], O_WRONLY) : STDOUT_FILENO;
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
  close(output_filedes);
  return 0;
}


