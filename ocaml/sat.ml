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
type satresult = R of (bool) | SR of (atomProp list * bool);;
exception Whoops of string;;

let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let negMatch = LR(Str.regexp "^~",NEG);;
let reglist = [varMatch;asterMatch;plusMatch;negMatch];;

let debug_flag = true;;
let debug x y =
  if debug_flag then
  let z = print_string (x^"\n\n") in
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
  if List.length l = 0 then n else List.hd l;;

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
    if String.length y = 0 then List.rev x else lex (lexwork x y 0);;

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
  if List.length ld = 1 then C(List.hd ld)
  else CF((List.hd ld),(makeCNF (List.tl ld)));;

let rec parseDisj tk = 
  let a = parseAtom tk in
  match a with
  | (b,c) ->
      if List.length c = 0 then (D(b),c) else
      if gettoken (List.hd c) = PLUS then
        let n = parseDisj (List.tl c) in
          match n with |
          (d,t) -> (DJ(b,d),t)
      else (D(b),c);;

let parseDisjuncts t =
  let rec h r tk = 
    if List.length tk = 0 then r else
    match parseDisj tk with |
    (m,n) -> if List.length n = 0 then m::r
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
  let s1 = atomlit rawa in
  let s2 = atomlit rawb in
  s1 = s2;;

let booldisagree a b =
  match a with |
  AP(s) ->
    begin
    match b with
    |AP (ss) -> false
    |NAG (n,ss) -> true
    end
  |NAG (n,s) ->
      begin
      match b with
      |AP(ss) -> true
      | NAG(nn,ss) -> false
      end
;;

let rec tolatomstr al = 
    match al with
    | [] -> ""
    | x::xs -> (toatomstr x)^" "^(tolatomstr xs)
    | _ -> "";;

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

let rec contradictsany ap ldj =
  match ldj with
  | CJ(app) -> (debug (toatomstr ap) (contradicts ap app))
  | MCJ(app,more) ->
      if contradicts ap app then true else contradictsany ap more;;

let distributetoconj d cj =
  match cj with
  | Cnt(c) -> cj
  | CONJ(c) -> 
      match c with
      | CJ(ap) ->
          if debug(toatomstr d^" "^(toatomstr ap)) (contradicts d ap) then Cnt(CONTRARY) else CONJ(MCJ(d,c))
      | MCJ(ap,more) -> 
          if (contradicts d ap) || (contradictsany d more) then Cnt(CONTRARY) else CONJ(MCJ(d,c))
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
                concatdnf (distributeover aap p) (helper ddj)
          in 
          let z = helper dj in
          concatdnf (distributeover ap p) z;;

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

let rec checksat x =
    match x with
    | J(Cnt(s)) -> false
    | J(CONJ(s)) -> true
    | DF(s,more) ->
    begin
    match s with
    | Cnt(n) -> checksat more
    | CONJ(n) -> true
    end

let dosat_all x =
    checksat (cnf2dnf x)

let getAtom x =
    match x with
    | AP(a) -> a;
    | NAG(n,a) -> a;;

let atomeq x y =
    (getAtom x) = (getAtom y);;

let rec unique l = 
    match l with
    | [] -> []
    | x::xs -> x::unique((List.filter (fun y -> not (atomeq x y)) xs));;

let rec getVars d = 
    match d with |
    D(ap) -> [ap] |
    DJ(ap,dj) -> ap::(getVars dj)

let rec getInitialAssignment x = 
    match x with
    | C(dj) -> unique(getVars(dj))
    | CF(dj,cf) -> unique(List.append (getVars dj) (getInitialAssignment cf));;


let rec satsatom a y =
    match y with
    | [] -> true
    | x::xs -> if (contradicts a x) then false else satsatom a xs;;

let rec satsdj d y =
    match d with 
    | D(ap) -> satsatom ap y 
    | DJ(ap,dj) ->  if satsatom ap y then true else satsdj dj y;;

let rec sats x y = 
    match x with
    | C(dj) -> satsdj dj y
    | CF(dj,cf) -> (satsdj dj y) && (sats cf y);;

let rec getFirstFail x y = 
    match x with 
    | C(dj) -> if not (satsdj dj y) then dj else raise (Whoops "foo")
    | CF(dj,cf) -> if not (satsdj dj y) then dj else getFirstFail cf y;;

let rec getFirstFailV dj y =
    match dj with
    | D(ap) -> if not (satsatom ap y) then ap else raise(Whoops "bad")
    | DJ(ap,djj) -> if not (satsatom ap y) then ap else getFirstFailV djj y;;

let rec flip v y =
    match y with
    | [] -> []
    | x::xs -> if contradicts v x then v::xs else x::(flip v xs);;

let getNextAssign x y =
    let dj = getFirstFail x y in
    let v = getFirstFailV dj y in
    flip v y;;

let rec trySat x y n =
   if n = 0 then R(false) else
       if sats x y then SR(y,true) else
           let na = getNextAssign x y in
           trySat x (debug ((Printf.sprintf "%d" n )^"\n"^(tolatomstr na)) na) (n-1);;

let rec exp b p r =
    if p == 0 then 1 else if p == 1 then r else exp b (p-1) (r*b);;

let min x y = 
    let n = exp 2 y 1 in
    if n < x then n else x;;

let dosat_naive x =
    let y = getInitialAssignment x in
    trySat x y (1+((min 1048576 (List.length y))));;


let fn = (read_line ());;
print_endline fn
let satfile = open_in fn;;
print_string "reading\n";;
let satinstance = input_line satfile;;
let test = PS([],satinstance);;
print_string "lexing\n";;
let z = lex test;;
print_string "parsing\n";;
let l = parse z;;
print_string "solving\n";;
let bo = dosat_naive l;;
match bo with R(b) -> 
if b then print_string "yes\n" else print_string "no\n"
|SR(al,b) -> print_string ("yes\n"^(tolatomstr al));;
