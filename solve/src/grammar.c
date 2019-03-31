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

int isWhite(char c) {
  return (c == '\t' || c == '\n' || c == ' '); 
}

size_t killWhite(char *source, size_t i, size_t ii) {
  for (;i<ii;i++) {
    char c = source[i];
    if isWhite(c)  {
      continue;
    }
    break;
  }
  return i;
}

struct token *tokenize(struct fi info) {
  size_t i = 0;
  struct token *r = malloc(sizeof(struct token));
  int n = 0;
  size_t ii = info.s;
  char c;
  char *source;
  struct token tok;
  for (;i<ii;i++) {
    i = killWhite(source,i,ii);
    if (i >= ii) { break; }
    r = realloc(r,n+1*(sizeof(struct token)));
    tok = r[n];
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
      tok.type = LCRULY;
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
      i = scanLit(source,i,ii,&r);
    }
  }
  return r;
}

int parse(struct fi * info) {
  if (info->m == (void *)-1) {
    return -1;
  }
  struct token *tokens = tokenize(*info);
  return 1;
}


int main(int argc, char** argv) {
  if (argc <2 ) {
    fprintf(stderr,"usage solve [fn]\n");
    return -1;
  }
  return parse(getFile(argv[1]));
}
