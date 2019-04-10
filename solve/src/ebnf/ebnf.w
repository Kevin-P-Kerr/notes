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
$$term=factor\lbrace "\vert" factor \rbrace.$$
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
alphanumeric. We also want to deal with whitespac
is an easy way.
Thus, we can produce the following
modifications.
$$syntax=\lbrace production \rbrace.$$
$$production=identifier "=" expression"."$$
$$expression=term\lbrace term \rbrace.$$
$$term=factor\lbrace "\vert" factor \rbrace.$$
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
@<set up headers@>@;
@<set up structs@>@;
@<v1 parsing routines@>@;
@<file io routines@>@;
@<main@>

@ @<set up headers@>=
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
@ Of these headers, {\it stdio} allows us to write 
input/and output for debugging and other purposes,
{\it stdlib} gives us access to {\it malloc} and 
{\it free}, and {\it stat, types} and {\it fctnl}
provide file i/o facilities through {\it mmap}
and friends.

@ @<main@>=
void printError(char *in, int i,int ii) {
  int j;
  int n=i-5;
  int nn = i+5;
  if (n < 0) {
    n=0;
  }
  if (nn > ii) {
    nn = ii;
  }
  j = n;
  for (;j<nn;j++) {
    if (in[j] == '\n') {
      fprintf(stderr,"\\n");
    }
    else {
      fprintf(stderr,"%c",in[j]);
    }
  }
  fprintf(stderr,"\n");
  j = n;
  for (;j<nn;j++) {
    if (j == i) {
      fprintf(stderr,"^");
    }
    else {
      fprintf(stderr," ");
    }
  }
  fprintf(stderr,"\n");
}

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: nauer [fn]\n");
    return -1;
  }
  return doParse(getFile(argv[1]));
}
@
Our first order of business is file i/o. To do this we will need to define a struct.

@ @<set up structs@>=
struct fi {
  void * m;
  size_t size;
  const char *fn;
};
enum nodeType {
  Syntax,Production,Expression,Term,Factor,Identifier,
  String,Character,Alpha};
struct parseNode {
  enum nodeType type;
  struct parseNode **children;
  size_t start; // inclusive
  size_t end; //exlusive;
  size_t numChildren;
};

struct parseNode *initNode(enum nodeType type, size_t start) {
  struct parseNode *pn = malloc(sizeof(struct parseNode));
  pn->type = type;
  pn->start = start;
  pn->numChildren = 0;
  return pn;
}

struct parseNode *ERROR = -1;

int addChild(struct parseNode *parent, 
struct parseNode *child) {
  if (child < 0) {
    return -1;
  }
  size_t n = parent->numChildren+1;
  struct parseNode **children = parent->children;
  children = realloc(children,sizeof(struct parseNode)*n);
  children[n-1] = child;
  parent->numChildren = n;
  return 1;
}

void freeNode(struct parseNode *p) {
  size_t i = 0;
  size_t ii = p->numChildren;
  for(;i<ii;i++) {
    struct parseNode *c = p->children[i];
    freeNode(c);
  }
  free(p);
}

@
Now we can write our file i/o routines.
@ @<file io routines@>=
size_t getFileSize(const char *fn) {
  struct stat st;
  stat(fn, &st);
  size_t r = st.st_size;
  return r;
}

struct fi *getFile(const char *fn) {
  size_t s = getFileSize(fn)/(sizeof(char));
  int fd = open(fn,O_RDONLY);
  void * m = mmap(NULL,s,PROT_READ,MAP_PRIVATE,fd,0);
  struct fi *info = malloc(sizeof(struct fi));
  info->m = m;
  info->size = s;
  info->fn = fn;
  return info;
}
@
Now we can begin the parsing the routines. 
The idea is to pass a reference to the index of 
the current character under 
consideration into subsequent routines.

@<v1 parsing routines@>=
@<entry routine@>@;
@<v1 production routine@>@;
@<v1 expression routine@>@;
@<v1 term routine@>@;
@<v1 factor routine@>@;
@<v1 identifier routine@>@;
@<v1 character routine@>@;
@<v1 string routine@>@;


@ @<entry routine@>=
void killWhite(char *in, int *i, int ii) {
  if (*i >= ii) {
    return;
  }
  int n = *i;
  for (;n<ii;n++) {
    char c = in[n];
    if (!(c == ' ' || c == '\n' || c == '\t')) {
      break;
    }
  }
  *i = n;
  return;
}

struct parseNode *doParse(struct fi *info) {
  if (info->size <= 0) {
    return -1;
  }
  char *in = info->m;
  size_t i = 0;
  size_t ii = info->size;
  struct parseNode *ast = initNode(Syntax,0);
  while(i<ii) {
    int status = addChild(ast,parseProduction(in,&i,ii));
    killWhite(in,&i,ii);
    if (i >= ii || status < 0) {
      return ERROR; 
    }
  }
  ast->end = i;
  return ast;
}

@ the idea of the entry routine is to
set up the state of the parser, and then 
{\it enter} into subsequent routines.

@ @<v1 production routine@>=
struct parseNode *parseProduction(char *in, int *i, int ii) {
  int status;
  struct parseNode *ast = initNode(Production,*i);
  status = addChild(ast,parseIdentifier(in,i,ii));
  if (status < 0) {
    return Error;
  }
  if (*i >= ii) {
    fprintf(stderr,"parseProduction: out of bounds\n");
    return Error;
  }
  killWhite(in,i,ii);
  char c = in[*i];
  if (c != '=') {
    fprintf(stderr,"parseProduction: expected '=', got '%c'\n",c);
    return ERROR;
  }
  *i = *i+1;
  status = addChild(ast,parseExpression(in,i,ii));
  if (status < 0) {
    return ERROR;
  }
  if (*i >= ii) {
    fprintf(stderr,"parseProduction2: out of bounds\n");
    return ERROR;
  }
  killWhite(in,i,ii);
  c = in[*i];
  if (c != '.') {
    fprintf(stderr, "parseProduction : expected '.', got '%c'\n'",c);
    return ERROR;
  }
  *i = *i+1;
  return ast;
}
@

@ @<v1 expression routine@>=
int parseExpression(char *in, int *i, int ii) {
  int status = parseTerm(in,i,ii);
  if (status < 0) {
    return status;
  }
  while (status >= 0) {
    status = parseTerm(in,i,ii);
  }
  return 1;
}
@

@ @<v1 term routine@>=
int parseTerm(char *in, int *i, int ii) {
  int status = parseFactor(in,i,ii);
  if (status < 0) {
    return status;
  }
  killWhite(in,i,ii);
  if (*i >= ii) {
    return status;
  }
  char c = in[*i];
  if (c == '|') {
    while (status >= 0 && c == '|' && *i < ii) {
      *i = *i+1;
      status = parseFactor(in,i,ii);
      killWhite(in,i,ii);
      c = in[*i];
    }
  }
  return 1;
}
@

@ @<v1 factor routine@>=
int parseFactor(char *in, int *i, int ii) {
  int status = parseIdentifier(in,i,ii);
  if (status >= 0) {
    return 1;
  }
  status = parseString(in,i,ii);
  if (status >= 0) {
    return 1;
  }
  killWhite(in,i,ii);
  if (*i >= ii) {
    fprintf(stderr,"parseFactor: out of bounds\n");
    return -1;
  }
  char c = in[*i];
  if (c == '(') {
    *i = *i+1;
    status = parseString(in,i,ii);
    if (status < 0) {
      return status;
    }
    killWhite(in,i,ii);
    c = in[*i];
    if (c != ')') {
      fprintf(stderr,"parseFactor: expected ')', got %c\n", c);
      return -1;
    }
    *i = *i+1;
    return 1;
  }
  if (c == '[') {
    status = parseExpression(in,i,ii);
    if (status < 0) {
      return status;
    }
    killWhite(in,i,ii);
    c = in[*i];
    if (c != ']') {
      fprintf(stderr,"parseFactor: expected ']', got %c\n",c);
      return -1;
    }
    *i = *i+1;
    return 1;
  }
  if (c == '{') {
    *i = *i+1;
    status = parseExpression(in,i,ii);
    if (status < 0) {
      return -1;
    }
    killWhite(in,i,ii);
    *i = *i+1;
    if (i >= ii) {
      return -1;
    }
    c = in[*i];
    if (c != '}') {
      return -1;
    }
    *i = *i+1;
  }
  return -1;
}

@
@ @<v1 identifier routine@>=
int parseIdentifier(char *in, int *i, int ii) {
  int status = parseAlpha(in,i,ii);
  if (status < 0) {
    return status;
  }
  while (status > 0) {
    status = parseCharacter(in,i,ii);
  }
  return 1;
}
@

@ @<v1 character routine@>=
int parseCharacter(char *in,int *i,int ii) {
  if (*i >= ii) {
    fprintf(stderr,"parseCharacter: index out of bounds\n");
    return -1;
  }
  killWhite(in,i,ii);
  char c = in[*i];
  // check for reserved characters
  if (c == '=' || c == '.' || c == '{' 
    || c == '}' || c == ')'  || c == '"'
    || c == '(' || c == ']' 
    || c == '[' || c == '|' || c == '.') {
    return -1;
  }
  if (c < '!' || c > '~') {
    fprintf(stderr,"parseCharacter: expected character in class !-~, got %d\n",c);
    return -1;
  }
  *i = *i+1;
  return 1;
}

int parseAlpha(char *in, int * i,int ii) {
  if (*i >= ii) {
    fprintf(stderr,"parseAlpha: index out of bound\n");
    return -1;
  }
  killWhite(in,i,ii);
  char c = in[*i];
  if (c < 'A' || c > 'z') {
    fprintf(stderr,"parseAlpha: expected character in class A-z, got %c\n",c);
    printError(in,*i,ii);
    return -1;
  }
  *i = *i+1;
  return 1;
}
@

@ @<v1 string routine@>=
int parseString(char *in,int *i, int ii) {
  killWhite(in,i,ii);
  if (*i >= ii) {
    fprintf(stderr,"parseString: index out of bound\n");
    return -1;
  }
  char c = in[*i];
  if (c != '"') {
    fprintf(stderr,"parseString: expected '\"', got %c\n",c);
    return -1;
  }
  *i = *i+1;
  int status = parseCharacter(in,i,ii);
  if (status < 0) {
    return status;
  }
  while (status >= 0) {
    status = parseCharacter(in,i,ii);
  }
  killWhite(in,i,ii);
  c = in[*i];
  if (c != '"') {
    fprintf(stderr,"parseString: no closing quote\n");
    return -1;
  }
  *i = *i+1;
  return 1;
}
@

@* Index.
