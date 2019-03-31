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

enum tokenType { PAREN, VAR, PLUST, ASTER, COLON };

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

struct token *tokenize(struct fi info) {
  size_t i = 0;
  struct token *r = malloc(sizeof(struct token));
  int n = 0;
  size_t ii = info.s;
  char c;
  char *source = (char *) info.m;
  struct token tok;
  for (;i<ii;i++) {
    tok = r[n];
    c = source[i];
    if (c == '*') { 
    }

  }

}

int parse(struct fi * info) {
  if (info->m == (void *)-1) {
    return -1;
  }
  struct token *tokens = tokenize(&info);
}


int main(int argc, char** argv) {
  if (argc <2 ) {
    fprintf(stderr,"usage solve [fn]\n");
    return -1;
  }
  return parse(getFile(argv[1]));
}
