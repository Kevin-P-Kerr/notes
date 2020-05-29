open Str;;
open List;;
open Random;;

type bexpr =  A of (int) | N of (bexpr) | M of (bexpr*bexpr) | P of (bexpr*bexpr);;
type 'a optional = EMPTY | R of ('a);;
type simple_truth_table_record = STTR of ((bool list)*bool);;
type complex_truth_table_record = CTTR of ((bool list)*(bool list));;

let map l f =
  let rec helper l f r =
  match l with
  |[] -> r
  |x::xs -> helper xs f r@[f x]
  in
  helper l f [];;

let collect l =
  let rec helper l r = 
    match l with
    |[] -> r
    |x::xs -> helper xs (r^x)
  in
  helper l "";;

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
  | N(x) -> "~("^(toStr x)^")"
  | M(x,y) -> "("^(toStr x)^")*("^(toStr y)^")"
  | P(x,y) -> "("^(toStr x)^")+("^(toStr y)^")";;

let row2bexpr l = 
  let rec helper l i =
    match l with
    | [] -> EMPTY
    | x::xs -> 
        let b = if x then A(i) else N(A(i)) in
        let y = helper xs (i+1) in
        match y with
        | EMPTY -> R(b)
        | R(x) -> R(M(b,x))
  in
  helper l 0;;

(* ttr = truth table record *)
let extractNot ttr = 
  match ttr with
  | STTR(l,v) ->
      if v then EMPTY else
        row2bexpr l;;

(* ttr = truth table record *)
let extractTrue ttr = 
  match ttr with
  | STTR(l,v) ->
      if not v then EMPTY else
        row2bexpr l;;

let collectNots i =
  let rec helper i r =
    match i with
    | [] -> r
    | x::xs -> 
        let b = extractNot x in
        match b with
        | EMPTY -> helper xs r
        | R(z) -> 
            match r with
            | EMPTY -> helper xs (R(z))
            | R(y) -> helper xs (R(P(z,y)))
  in
  helper i EMPTY;;

let collectTrues i =
  let rec helper i r =
    match i with
    | [] -> r
    | x::xs -> 
        let b = extractTrue x in
        match b with
        | EMPTY -> helper xs r
        | R(z) -> 
            match r with
            | EMPTY -> helper xs (R(z))
            | R(y) -> helper xs (R(P(z,y)))
  in
  helper i EMPTY;;

(* learn a boolean function in the simplest way *)
let learn1bit i =
  let ns = collectNots i in
  let ands = collectTrues i in
  match ns with
  | EMPTY -> ands
  | R(x) -> 
      match ands with
      | EMPTY -> R(N(x))
      | R(y) -> R(P(N(x),y));;

exception IndexError of string;;
let getNth r l = 
  let rec helper r l =
    match l with
    | [] -> raise (IndexError "getNth")
    | x::xs ->
        if r=0 then x else helper (r-1) xs
  in
  helper r l;;

let getLen l = 
  let rec helper l r = 
    match l with
    | [] -> r
    | x::xs -> helper xs (r+1)
  in
  helper l 0;;

let unRollToSTTR i = 
  let rec helperA i n r = 
    match i with
    | [] -> r
    | x::xs ->
        match x with
        | CTTR(a,rr) ->
            let nr = STTR(a,(getNth n rr)) 
            in
            helperA xs n r@[nr]
  in
  let rec helperB i n l r =
    if n=l then r else
      helperB i (n+1) l (r@[helperA i n []])
  in
  match i with
  | [] -> []
  | x::xs ->
      match x with
      |CTTR(y,z) -> 
          let len = getLen z in
          helperB i 0 len [];;



(* learn a more complex function *)
let learnNbit i = 
  let unrolled = unRollToSTTR i in
  let rec helper l r =
    match l with 
    |[] -> r
    | x::xs -> 
        let f = learn1bit x in
        helper xs r@[f]
  in
  helper unrolled [];;


let printEval f a = 
  if evalBoolExpr f a then "true" else "false";;

let printBoolSeq l = 
  let rec helper l r = 
    match l with
    | [] -> r
    | x::xs ->
       let j = if x then "1" else "0" in
       helper xs r^j
  in
  helper l "";;

let toBoolSeq l a =
  let rec helper l a r =
    match l with
    |[] -> r
    |f::xs ->  let j = if evalBoolExpr f a then 1 else 0 in 
    helper xs a r@[j]
  in
  helper l a [];;


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

(* debugging and testing: TODO: put this in it's own file *)
let xor a b = 
  let r = eval_relation [a;b] [6]
  in 
  List.hd r;;

let d = xor true false;;

let dbg s =
  print_string (s^"\n");;


let partxor = [STTR([false;true],true);STTR([true;false], true)];;

let pbe = learn1bit partxor;;
let myf = match pbe with
| EMPTY -> let y = print_string "error error\n" in A(0)
| R(x) -> let y =  print_string ((toStr x)^"\n") in x;;

(*
dbg (printEval myf [|false;false|]);;
dbg (printEval myf [|false;true|]);;
dbg (printEval myf [|true;false|]);;
dbg (printEval myf [|true;true|]);;
*)

let printTruthTable l = 
  let f y = 
    match y with 
    |CTTR(j,k) -> 
        let n = printBoolSeq j
        in
        let o = printBoolSeq k
        in
        n^":"^o^"\n"
  in
  map l f;;

let int2bool i max = 
  let rec helper i p r =
    if p > max then r else
    let b = (p land i)>0 in
    helper i (p*2) r@[b]
  in
  helper i 1 [];;
    

let makeTruthTableForAddition n m =
  let max = (pow2 n)-1 in
  let rec helper m r =
    if m=0 then r else
      let j = Random.int max in
      let k = Random.int max in
      let l = (j+k) mod max in
      let  () = print_int j in
      let () = print_string "\n" in
      let () = print_int k in
      let () = print_string "\n" in
      let () = print_int l in
      let () = print_string "\n**\n" in
      let jj = int2bool j max in
      let kk = int2bool k max in
      let ll = int2bool l max in
      let record = CTTR((List.append jj kk), ll) in
      helper (m-1) r@[record]
  in
  helper m [];;

let fourBit = makeTruthTableForAddition 4 4;;
let tt = printTruthTable fourBit;;
dbg (collect tt);;

dbg (printBoolSeq (int2bool 4 15));;
dbg (printBoolSeq (int2bool 7 15));;
dbg (printBoolSeq (int2bool 12 15));;
      
