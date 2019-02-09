open Str;;
open List;;

type tokentype = WHITE | LPAREN | RPAREN | LCURLY | RCURLY | LBRAK | RBRAK | DASH | QUOTE | PLUS | ASTER | SLASH | POUND | VAR | EQUAL;;
type token = TOK of (tokentype*string);;
type lexrule = LR of (Str.regexp * tokentype);;
type tokenstackcommand = PEEK|POP;;

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
let itertoken s = 
  let rec helper a =
    match a with 
    | [] -> LRN 
    | x::xs -> 
        match x with 
        | LR(r,t) ->
            if ismatch r s then x else helper xs
  in
  helper reglist;;

let tokenize s =
  let rec makeTokens x y = 
    if String.length x = 0 then y else 
      let lexstate = itertoken x in
      match lexstate with 
      | LRN -> []
      | LR(r,t) -> 
        let m = getmatch x in
        let ns = getnonempty (Str.split r x) "" in
        if t = WHITE then makeTokens ns y else
          makeTokens ns (LT(t,m)::y)
  in
  let ts = makeTokens s []
  in List.rev ts;;

