open Str;;
open List;;

type token = ASTER | PLUS | MINUS | VAR | ONE | ZERO | WHITE;;
type lexrule = LR of (Str.regexp * token);;
type lextoken = LT of (token*string);;

let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let minusMath = LR(Str.regexp "^-",MINUS);;
let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let oneMatch = LR(Str.regexp "^1",ONE);;
let zeroMatch = LR(Str.regexp "^0",ZERO);;
let whiteRE = Str.regexp "[ \n\r\t]+";;
let whiteMatch = LR(whiteRE,WHITE);;
let reglist = [varMatch;asterMatch;plusMatch;zeroMatch;oneMatch];;

let ismatch r s =
  Str.string_match r s 0;;

let getmatch y =
  Str.matched_string y;;

let getnonempty l n = 
  if List.length l = 0 then n else List.hd l;;

let tokenize s =
  let rec makeTokens x y = 
    if String.length x = 0 then y else 
          
let rec repl a = 
  let s = read_line () in
  let ts = tokenize s in
  print_string (s^"\n");
  repl ();;

repl ()
