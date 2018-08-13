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
type conj = CJ of atomProp | MCJ of (atomProp*conj);;
type contrary = CONTRARY;;
type satconj = Cnt of contrary | CONJ of conj;;
type dnf = J of satconj | DF of (satconj*dnf);;

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

let getAtom x = match x with | AP(a) -> a;;
let gettoken x = match x with | T(t,s) -> t;;
let getstr x = match x with | T(t,s) -> s;;

let ismatch r s =
  Str.string_match r s 0;;

let getmatch y =
  Str.matched_string y;;

let getnonempty l n = 
  if List.length l == 0 then n else List.hd l;;

let rec lexwork x y z=
  let LR(r,t) = List.nth reglist z in
  if ismatch r y 
    then 
    let m = getmatch y in
    let l = Str.split r y in
    let ns = getnonempty l "" in
      PS(T(t,m)::x,ns) 
    else lexwork x y (z+1);;
   

let rec lex n =
  match n with 
   | PS(x, y) ->
    if String.length y == 0 then List.rev x else lex (lexwork x y 0);;

let rec parseAtom tk = 
  let t = List.nth tk 0 in
  let f = gettoken t in
  let s = getstr t in
  let tail = List.tl tk in
  match f with 
    | VAR -> (AP(A(s)),tail)
    | NEG -> 
        let r = parseAtom tail in
        match r with |
        (y,z) -> (NAG(NEGATE,getAtom(y)),z);;

let rec makeCNF ld = 
  if List.length ld == 1 then C(List.hd ld)
  else CF((List.hd ld),(makeCNF (List.tl ld)));;

let rec parseDisj tk = 
  let a = parseAtom tk in
  match a with
  | (b,c) ->
      if List.length c == 0 then (D(b),c) else
      if gettoken (List.hd c) == PLUS then
        let n = parseDisj (List.tl c) in
          match n with |
          (d,t) -> (DJ(b,d),t)
      else (D(b),c);;

let parseDisjuncts t =
  let rec h r tk = 
    if List.length tk == 0 then r else
    match parseDisj tk with |
    (m,n) -> if List.length n == 0 then m::r
    (* pop the aster off here *)
    else h (m::r) (List.tl n)
  in
  h [] t;;

let parse t =
  makeCNF(List.rev(parseDisjuncts t));;

let atomlit a = 
  match a with
  | A(s) -> s;;

let toatomstr a =
  match a with 
  | AP(a) -> atomlit a
  | NAG(n,a) -> "~"^(atomlit a);;

let rec todisjstr d = 
  match d with
  | D(a) -> toatomstr a
  | DJ(a,j) -> (toatomstr a)^"+"^(todisjstr j)

let rec tocnfstr cnf = 
  match cnf with
  | C(d) -> todisjstr d
  | CF(d,c) -> (todisjstr d)^"*"^(tocnfstr c);;



let getraw a =
  match a with 
  | AP(s) -> s
  | NAG(n,s) -> s
;;

let litequals a b =
  let rawa = getraw a in
  let rawb = getraw b in
  atomlit rawa == atomlit rawb;;

let booldisagree a b =
  match a with |
  AP(s) ->
    match b with
    |AP (ss) -> false
    |NAG (n,ss) -> true
  | NAG(n,s) ->
      match b with
      |AP(ss) -> true
      | NAG(nn,ss) -> false
;;

let contradicts a b = 
   ((litequals a b) && (booldisagree a b));;

let rec concatdnf d1 d2 = 
  match d1 with
  |J(s) -> DF(s,d2)
  | DF(s,d) -> 
      let d3 = concatdnf d d2 in 
      DF(s,d3);;

let rec echocnf2dnf d = 
  match d with 
  | C(D(d)) -> J(CONJ(CJ(d)))
  | C(DJ(a,b)) -> DF(CONJ(CJ(a)), echocnf2dnf (C(b)));;

let distributetoconj d cj =
  match cj with
  | Cnt(c) -> cj
  | CONJ(c) -> CONJ(MCJ(d,c))
;;

let rec distributeover d p =
  match p with 
  | J(cj) -> J(distributetoconj d cj)
  | DF(cj,df) -> 
      let nc = distributetoconj d cj in
      let rest = distributeover d df in
      DF(nc,rest);;

let rec cnf2dnf c = 
  match c with 
  | C(d) -> echocnf2dnf c
  | CF(d,e) ->
      let p = cnf2dnf e in
      match d with 
      | D(ap) -> (distributeover ap p)
      | DJ(ap,dj) ->
          let rec helper dis =
            match dis with
            | D(at) -> distributeover at p
            | DJ(aap,ddj) -> 
                DF((distributeover aap p),(helper ddj))
          in 
          let z = helper dj in
          DF((distributeover ap e),z);;

let rec print_conj cj = 
  match cj with
  | CJ(ap) -> toatomstr ap
  | MCJ(ap,cj) -> (toatomstr ap)^"*"^print_conj cj;;

let print_satconj c =
  match c with |
  Cnt(z) -> "0"
  | CONJ(cj) -> print_conj cj;;

let rec print_dnf d = 
  match d with |
  J(s) -> print_satconj s
  | DF(s,dd) -> (print_satconj s)^"+"^(print_dnf dd);;

let test = PS([],"a+b*d+z");;
let z = lex test;;
let l = parse z;;

let n = cnf2dnf l;;
print_string(print_dnf n);;
