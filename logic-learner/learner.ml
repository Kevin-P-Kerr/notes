open Str;;
open List;;

let pow2 b = 
  let rec helper x r = 
    if x=0 then 1 else if x=1 then r*2 else let z = x-1 in let rr = r*2 in helper z rr
  in 
  helper b 1;;

let toInt boolVec =
  let rec helper bv d r =
    match bv with
    | [] -> r
    | x::xs -> let nd = d+1 in
        if x then helper xs nd (r + (pow2 d)) else helper xs nd r
  in
  helper boolVec 0 0;;

let getNthBit n b = 
  let p = pow2 n in
  b land p > 0;;


(* a truth table is an 2^n bit integer representation of a boolean function of n variables
 * where we say that the leftmost bit is the least significant 
 * for example, the truth table for AND is 0001 = 8 
*)
let eval_relation args truthTable = 
  let n = toInt(args) in
  getNthBit n truthTable;;

let xor a b = 
  eval_relation [a;b] 6;;

let n a b = 
  eval_relation [a;b] 1;;

let d = xor true true;;
print_string "\n";;
if d then print_string "true" else print_string "false";;
print_string "\n";;
