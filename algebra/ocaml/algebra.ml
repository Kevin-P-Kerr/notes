open Str;;
open List;;

type tokentype = WHITE | LPAREN | RPAREN | LCURLY | RCURLY | LBRAK | RBRAK | DASH | QUOTE | PLUS | ASTER | SLASH | POUND | VAR | EQUAL;;
type token = TOK of (tokentype*string);;
type lexrule = LR of (Str.regexp * tokentype);;

let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let minusMatch = LR(Str.regexp "^-",DASH);;
let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let equalMatch = LR(Str.regexp "^=",EQUAL);;
let lparenMatch = LR(Str.regexp "^(",LPAREN);; 
let rparenMatch = LR(Str.regexp "^)",RPAREN);; 
let slashMatch = LR(Str.regexp "^/",SLASH);; 
let lcurlyMatch = LR(Str.regexp "^{",LCURLY);; 
let rcurlyMatch = LR(Str.regexp "^}",RCURLY);; 
let lbrakMatch = LR(Str.regexp "^[",LBRAK);; 
let rbrakMatch = LR(Str.regexp "^]",RBRAK);; 
let poundMatch  = LR(Str.regexp "^#",POUND);; 
let quoteMatch  = LR(Str.regexp "^\"",QUOTE);; 
let whiteRE = Str.regexp "^[ \n\r\t]+";;
let whiteMatch = LR(whiteRE,WHITE);;
let lex s = 
  let rec helper s a = 
    if String.length s = 0 then a else


