open Str;;
open List;;

type tokentype = LPAREN | RPAREN | LCURLY | RCURLY | LBRAK | RBRAK | DASH | QUOTE | PLUS | ASTER | SLASH | POUND;;
type token = TOK of (tokentype*string);;


let rec lex s =

