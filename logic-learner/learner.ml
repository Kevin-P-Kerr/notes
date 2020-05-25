open Str;;
open List;;

type bexpr = A of (int) | N of (bexpr) | M of (bexpr*bexpr) | P of (bexpr*bexpr);;


let rec evalBoolExpr e v =
  match e with
  | A(x) -> v.(x)
  | N(x) -> not (evalBoolExpr x v)
  | M(x,y) -> (evalBoolExpr x v) && (evalBoolExpr y v)
  | P(x,y) -> (evalBoolExpr x v) || (evalBoolExpr y v);;

let toAlpha x = 
  let r = [|"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o"|] in
  r.(x);;

let rec toStr e =
  match e with
  | A(x) -> toAlpha x
  | N(x) -> "~"^(toStr x)
  | M(x,y) -> "("^(toStr x)^")*("^(toStr y)^")"
  | P(x,y) -> "("^(toStr x)^")+("^(toStr y)^")";;

let myBoolExpr = M(N(A(0)),M(A(1),P(N(A(0)),A(2))));;

let j = evalBoolExpr myBoolExpr [|false; false;|];;
if j then print_string "true" else print_string "\nfalse";;
print_string ("\n"^(toStr myBoolExpr));;

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


(* a truth table is an m sized list of (2^n bit integers) representation of a boolean 
 * function of n input variables and m output variables
 * where we say that the leftmost bit is the least significant 
 * for example, the truth table for AND is 0001 = 8 
*)
let eval_relation args truthTable = 
  let n = toInt(args) in
  let rec helper t r =
    match t with 
    | [] -> r
    | x::xs -> let lr = getNthBit n x in helper xs (r@[lr])
  in helper truthTable [];;

let xor a b = 
  let r = eval_relation [a;b] [6]
  in 
  List.hd r;;

let d = xor true false;;
print_string "\n";;
if d then print_string "true" else print_string "false";;
print_string "\n";;
