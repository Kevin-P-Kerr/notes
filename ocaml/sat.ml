open Str;;
open List;;

type token = ASTER | PLUS | VAR | NEG;;
type tokenInfo = T of (token * string);;
type atom = A of string;;
type negate = NEGATE;;
type atomProp = AP of atom | NAG of (negate*atom)
type disj = D of atomProp | DJ of (atomProp*disj)
type cnf = C of disj | CF of (disj*cnf)
type lexrule = LR of (Str.regexp * token);;
type parsestate = PS of (tokenInfo list * string);;

let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let negMatch = LR(Str.regexp "^~",NEG);;
let reglist = [varMatch;asterMatch;plusMatch;negMatch];;

let debug_flag = true;;
let debug x y =
  if debug_flag then
  let z = print_string (x^"\n") in
  y
  else 
  y
;;

let ismatch r s =
  Str.string_match r s 0;;

let getmatch y =
  debug ("get it\n"^y) Str.matched_string y;;

let getnonempty l n = 
  if List.length l == 0 then n else List.hd l;;

let rec parsework x y z=
  let LR(r,t) = List.nth reglist z in
  if ismatch r y 
    then 
    let m = getmatch y in
    let l = Str.split r y in
    let ns = getnonempty l "" in
      PS(T(t,m)::x,ns) 
    else parsework x y (z+1);;
   

let rec parse n =
  match n with 
   | PS(x, y) ->
    if String.length y == 0 then x else parse (parsework x y 0);;

let test = PS([],"a+b*d");;

let z = parse test;;
