@*
\def\title{Fib}
This is a sample literate programming file.  
We 
will simply compute the first $20$ fibonacci numbers.

@p
#include<stdio.h>
@<calculate...@>
@<main@>


@ @<calculate a fibonacci number@>=
size_t fib(size_t i) {
  size_t n = 0;
  size_t nn = 1;
  size_t ii = i;
  i = 0;
  for (;i<ii;i++) {
    size_t m = n;
    n = nn;
    nn = nn+m;
  }
  return n;
}

@ @<main@>=
int main(void) {
  size_t i = 0;
  size_t ii = 20;
  for (;i<ii;i++) {
    size_t r =fib(i);
    fprintf(stdout,"%d:%d\n",(int) i, (int) r);
  }
  return 1;
}

@*  Index.
