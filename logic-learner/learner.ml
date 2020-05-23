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


(* a truth table is an 2^n bit integer representation of a boolean function of n variables
 * where we say that the leftmost bit is the least significant 
 * for example, the truth table for AND is 0001 = 8 
let eval_relation args truthTable = 
  let n = toInt(args) in
  lookUpVal(n truthTable);;
*)

