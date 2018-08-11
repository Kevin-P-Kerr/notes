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

let ismatch r s =
  Str.string_match r s 0;;

let rec n x y z=
  let LR(r,t) = List.nth(reglist z) in
  if ismatch r y then PS(T(t,Str.matched_string),List.hd(Str.split(r,y))) else n x y (z+1);;
   

let rec parse n =
  match x with 
   | PS(x, y) ->
    if String.length y == 0 then x else parse n x y 0;;


