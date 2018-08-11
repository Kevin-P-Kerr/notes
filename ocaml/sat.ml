open Str;;

type token = ASTER | PLUS | VAR | NEG;;
type tokenInfo = (T of token * S of string);;
type atom = A of string;;
type negate = NEGATE;;
type atomProp = AP of atom | NAG of (negate*atom)
type disj = D of atomProp | DJ of (atomProp*disj)
type cnf = C of disj | CF of (disj*cnf)

let varMatch = Str.regexp "^[A-Za-z]+";;
let asterMatch = Str.regexp "\\*";;
let plusMatch = Str.regexp "\\+";;
let negMatch = Str.regexp "~";;

let rec parse x y = 
  if String.length y == 0 then x else 
    if (


