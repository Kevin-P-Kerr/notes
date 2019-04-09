@* 
\def\title{Programming an EBNF Parser}
The aim of the program.

We wish to write a program that takes a description 
of a language and outputs a
function $$f(x) \rightarrow \lbrace 0 \vert 1 \rbrace $$ where 
x is a string of finite length, 
and 0 and 1 are the usual boolean values 
indicating if the string conforms to our grammar.

\beginsection
EBNF Definition

Our first order of business is to describe, as precisely as possible, 
the format in which we shall 
describe the grammars we wish to parse.  
Indeed, specifying such a format within that format 
itself is a good litmus test of its generality and power.

The notation we shall adopt is called EBNF--that is, 
Extended Backus-Nauer Form. This is given in N. Wirth's
{\it Compiler Construction}, as follows:
$$syntax=\lbrace production \rbrace.$$
$$production=identifier "=" expression"."$$
$$expression=term\lbrace term \rbrace.$$
$$term=factor\lbrace factor \rbrace.$$
$$factor=identifier\vert string \vert "(" expression ")"
\vert "\lbrack" expression "\rbrack" \vert 
"\lbrace" expression "\rbrace".$$
$$identifier=letter\lbrace digit\vert letter \rbrace.$$
$$string="""\lbrace character \rbrace"""$$
$$letter="A".\vert.."Z"$$
$$digit="0".\vert.."9"$$
In this grammer {\it $\lbrace expression \rbrace$} 
indicates that {\it this expression is repeated $0$ 
or more times}.
This is a useful grammer, except that character classes
are not available to us, and we would like to include
strings and character literals that are not simply
alphanumeric.  There, we can produce the following
modifications.
$$syntax=\lbrace production \rbrace.$$
$$production=identifier "=" expression"."$$
$$expression=term\lbrace term \rbrace.$$
$$term=factor\lbrace factor \rbrace.$$
$$factor=identifier\vert string \vert "(" expression ")"
\vert "\lbrack" expression "\rbrack" \vert 
"\lbrace" expression "\rbrace".$$
$$identifier=alpha\lbrace character\rbrace.$$
$$character=\lbrack !-\tilde\rbrack.$$
$$alpha=\lbrack A-z\rbrack.$$
$$string="""character\lbrace character\rbrace""".$$
Here, {\it $\lbrack !-\tilde\rbrack$} indicates 
{\it any character from 
ascii code 21 to ascii code 126, inclusive}.

Having this specification in hand, we can begin to construct our parser generator.  

The usual way to construct such a parser is through indirection.  
The parser itself does not operate upon an input 
stream of characters, but upon a stream of {\it tokens} 
which represent higher level 
syntatic constructs.  

However, this is not strictly necessary, 
and we will do away here with this layer of abstraction.

@
@p
@<set up headers@>
@<main@>

@ @<set up headers@>=
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fctnl.h>
@ Of these headers, {\it stdio} allows us to write 
input/and output for debugging and other purposes,
{\it stdlib} gives us access to {\it malloc} and 
{\it free}, and {\it stat, types} and {\it fctnl}
provide file i/o facilities through {\it mmap}
and friends.

@ @<main@>=
int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: nauer [fn]\n");
    return -1;
  }
  return doParse(getFile(argv[1]));
}
@


@* Index.
