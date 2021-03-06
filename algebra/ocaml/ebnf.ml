open Str;;
open List;;

type tokentype = WHITE | LPAREN | RPAREN | LCURLY | RCURLY | LBRAK | RBRAK | DASH | QUOTE | PLUS | ASTER | SLASH | POUND | VAR | EQUAL | BAR;;
type token = TOK of (tokentype*string);;
type lexrule = LR of (Str.regexp * tokentype) | LRN;;
type tokenstackcommand = PEEK|POP;;
type construct = EBNF | PRODUCTION | VARNAME | RULE | MANDATORY_SEQUENCE | OPTIONAL_SEQUENCE | SEQUENCE | CHARCLASS | STRING 
type astnode = ASTN of (construct*string) | ASTNE of (construct)
type ast = AST of (astnode*ast list) | EAST of (astnode)

exception parseError of string

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
let barMatch  = LR(Str.regexp "^|",BAR);; 
let whiteRE = Str.regexp "^[ \n\r\t]+";;
let whiteMatch = LR(whiteRE,WHITE);;
let reglist = [minusMatch;whiteMatch;varMatch;asterMatch;plusMatch;equalMatch;lparenMatch;rparenMatch;];;
let powotwo n =
    let rec helper n r =
        if n=0 then r else helper(n-1) (r*2)
    in
    helper n 1;;

let ismatch r s =
  Str.string_match r s 0;;

let getmatch y =
  Str.matched_string y;;

let getnonempty l n = 
  if List.length l = 0 then n else List.hd l;;
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
          makeTokens ns (TOK(t,m)::y)
  in
  let ts = makeTokens s []
  in List.rev ts;;

(* parsing routines *)

let empty t c =
  let node = ASTNE(EBNF) in
  let a = EAST(node)
  a;;

let parseProduction t c =
let parseEBNF t c =
  match t with
  |[] -> c t empty


let parse s = 
  let t = tokenize s in
  parseEBNF t;;
