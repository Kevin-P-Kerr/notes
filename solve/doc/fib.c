/*1:*/
#line 6 "fib.w"

#include<stdio.h> 
/*2:*/
#line 12 "fib.w"

size_t fib(size_t i){
size_t n= 0;
size_t nn= 1;
size_t ii= i;
i= 0;
for(;i<ii;i++){
size_t m= n;
n= nn;
nn= nn+m;
}
return n;
}

/*:2*/
#line 8 "fib.w"

/*3:*/
#line 26 "fib.w"

int main(void){
size_t i= 0;
size_t ii= 20;
for(;i<ii;i++){
size_t r= fib(i);
fprintf(stdout,"%d:%d\n",(int)i,(int)r);
}
return 1;
}

/*:3*/
#line 9 "fib.w"



/*:1*/
