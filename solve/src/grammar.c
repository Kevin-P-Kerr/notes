#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

struct fi {
  void * m;
  size_t s;
  const char *fn;
};

enum tokenType { EQUALS, VAR, RCURLY, LCURLY, BAR, QUOTE, LEFTSLASH  };

struct token {
  enum tokenType type;
  int start;
  int end;
};

struct tokenList {
  struct token *tokens;
  size_t size;
};

size_t getFileSize(const char *fn) {
  struct stat st;
  stat(fn, &st);
  size_t r = st.st_size;
  return r;
}

struct fi *getFile(const char *fn) {
  size_t s = getFileSize(fn);
  int fd = open(fn,O_RDONLY);
  void * m = mmap(NULL,s,PROT_READ,MAP_PRIVATE,fd,0);
  struct fi *info = malloc(sizeof(struct fi));
  info->m = m;
  info->s = s;
  info->fn = fn;
  return info;
}

int isWhite(char c) {
  return (c == '\t' || c == '\n' || c == ' '); 
}

size_t scanLit(char *source, size_t i, size_t ii, struct token *tok) {
  tok->type = VAR;
  tok->start =i;
  char c;
  for (;i<ii;i++) {
    c = source[i];
    if (c == '=' || c == '}' || c == '{' || c == '|' || c== '"' || isWhite(c)) {
      break;
    }
  }
  tok->end=i;
  return i;
}


size_t killWhite(char *source, size_t i, size_t ii) {
  for (;i<ii;i++) {
    char c = source[i];
    if (isWhite(c))  {
      continue;
    }
    break;
  }
  return i;
}

struct tokenList tokenize(struct fi info) {
  struct tokenList tl;
  size_t i = 0;
  struct token *r = malloc(sizeof(struct token));
  int n = 0;
  size_t ii = info.s;
  char c;
  char *source = (char *) info.m;
  struct token tok;
  for (;i<ii;i++) {
    i = killWhite(source,i,ii);
    if (i >= ii) { break; }
    r = realloc(r,(n+1)*(sizeof(struct token)));
    c = source[i];
    if (c == '=') { 
      tok.type = EQUALS;
      tok.start = i;
      tok.end = i+1;
    }
    else if (c == '}') {
      tok.type = RCURLY;
      tok.start = i;
      tok.end = i+1;
    }
    else if (c == '{') {
      tok.type = LCURLY;
      tok.start = i;
      tok.end = i+1;
    }
    else if (c == '|') {
      tok.type = BAR;
      tok.start = i;
      tok.end = i+1;
    }
    else if (c == '"') {
      tok.type = QUOTE;
      tok.start = i;
      tok.end = i+1;
    }
    else if (c == '\\') {
      tok.type = LEFTSLASH;
      tok.start = i;
      tok.end = i+1;
    }
    else {
      i = scanLit(source,i,ii,&tok);
      i--;
    }
    r[n] = tok;
    n++;
  }
  tl.tokens = r;
  tl.size = n;
  return tl;
}

void printTokens(struct tokenList l, char *source) {
  size_t i  = 0;
  size_t ii = l.size;
  struct token t;
  for (;i<ii;i++) {
    t = l.tokens[i];
    if (t.type == EQUALS) {
      fprintf(stdout,"token: EQUALS ");
    }
    if (t.type == VAR) {
      fprintf(stdout,"token: VAR ");
    }
    if (t.type == RCURLY)  {
      fprintf(stdout,"token: RCURLY ");
    }
    if (t.type == LCURLY) {
      fprintf(stdout,"token: LCURLY ");
    }
    if (t.type == BAR) {
      fprintf(stdout,"token: BAR ");
    }
    if (t.type == QUOTE) {
      fprintf(stdout,"token: QUOTE ");
    }
    if (t.type == LEFTSLASH) {
      fprintf(stdout,"token: LEFTSLASH ");
    }
    char *c = malloc(sizeof(char)*(t.end-t.start));
    int n = t.start;
    int nn = 0;
    for (;n<t.end;n++) {
      c[nn]= source[n];
      nn++;
    }
    fprintf(stdout, "lit: %s\n",c);
    free(c);
  }
}

size_t parseLeftHand(struct tokenList *tl, size_t i) {
  struct token t = tl->tokens[i];
  if (t.type != VAR) {
    return -1;
  }
  return i+1;
}

size_t parseProductionRule(struct tokenList *tl, size_t i) {
  return i+1;
}

int parse (struct tokenList *tl) {
  size_t i =0;
  while (i >=0 && i < tl->size) {
    i = parseLeftHand(tl,i);
    if (i < 0 ) {
      return -1;
    }
    struct token t = tl->tokens[i];
    if (t.type != EQUALS) {
      return -1;
    }
    i++;
    i = parseProductionRule(tl,i);
    if (i < 0) {
      return -1;
    }
  }
  return 1;
}


int doParse(struct fi * info) {
  if (info->m == (void *)-1) {
    return -1;
  }
  struct tokenList tokens = tokenize(*info);
  return parse(&tokens);
}


int main(int argc, char** argv) {
  if (argc <2 ) {
    fprintf(stderr,"usage solve [fn]\n");
    return -1;
  }
  return doParse(getFile(argv[1]));
}
