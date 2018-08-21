open Str;;
open List;;
open Random;;

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
type satbool = TRUE|FALSE|INDIFF;;
exception Whoops of string;;

Random.self_init();;

let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let negMatch = LR(Str.regexp "^~",NEG);;
let reglist = [varMatch;asterMatch;plusMatch;negMatch];;

let debug_flag = true;;
let debug x y =
  if debug_flag then
  let z = print_string ("debug: "^x^"\n") in
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

let litequals a b =
  let rawa = getraw a in
  let rawb = getraw b in
  let s1 = atomlit rawa in
  let s2 = atomlit rawb in
  s1 = s2;;

let contradicts a b = 
   ((litequals a b) && (booldisagree a b));;

let rec contradictsany ap ldj =
  match ldj with
  | CJ(app) -> (debug (toatomstr ap) (contradicts ap app))
  | MCJ(app,more) ->
      if contradicts ap app then true else contradictsany ap more;;

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

let rec echoDisj2DnfIter dj df =
  match dj with
  | D(at) -> DF(CONJ(CJ(at)),df)
  | DJ(at,djj) -> (echoDisj2DnfIter djj (DF(CONJ(CJ(at)),df)));;

let echoDisj2Dnf dj =
  match dj with
  | D(at) -> J(CONJ(CJ(at)))
  | DJ(at,djj) -> (echoDisj2DnfIter djj (J(CONJ(CJ(at)))));;

let rec concatDJ d1 d2 = 
    match d1 with
    | J(s) -> DF(s,d2)
    | DF(s,df) -> DF(s,(concatDJ df d2));;

let ontoconj a c =
    match c with
    |CJ(ap) -> if contradicts a ap then Cnt(CONTRARY) else CONJ(MCJ(a,c))
    | MCJ (ap,cj) ->
        if contradicts a ap then Cnt(CONTRARY) else 
            if contradictsany a cj then Cnt(CONTRARY) else 
                CONJ(MCJ(a,c));;

let onto a c = 
    match c with
    | Cnt(cnt) -> Cnt(cnt)
    | CONJ(con) -> ontoconj a con;;

let rec distribAP a d =
    match d with 
    | J(s) -> J(onto a s)
    | DF(s,df) -> DF((onto a s),(distribAP a df));;

let rec distribDJIter dj d r =
    match dj with
    | D(ap) -> concatDJ(distribAP ap d ) r
    | DJ(ap,djj) -> distribDJIter djj d (concatDJ (distribAP ap d) r);;

let distribDJ dj d = 
    match dj with
    | D(ap) -> distribAP ap d
    | DJ(ap,djj) -> 
        let x = distribAP ap d in
        distribDJIter djj d x

let rec distribCoverD c d =
    match c with 
    | C(dj) -> distribDJ dj d
    | CF(dj,cj) -> distribCoverD cj (distribDJ dj d);;

let cnf2dnf c = 
  match c with 
  | C(d) -> echoDisj2Dnf d
  | CF(d,e) ->
    let df = echoDisj2Dnf d in
    distribCoverD e df;;

let rec tolatomstr al = 
    match al with
    | [] -> ""
    | x::xs -> (toatomstr x)^" "^(tolatomstr xs)
    | _ -> "";;

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

let rec getFailV dj y =
  let b = Random.bool() in 
  match dj with
  | D(a) -> a
  | DJ(ap,djj) -> if b then ap else getFailV djj y;;

let rec flip v y =
    match y with
    | [] -> []
    | x::xs -> if contradicts v x then v::xs else x::(flip v xs);;

let getNextAssign x y =
    let dj = getFirstFail x y in
    let v = getFailV dj y in
    flip v y;;

let rec trySat x y n =
   if n = 0 then R(false) else
       if sats x y then SR(y,true) else
           let na = getNextAssign x y in
           trySat x (debug ((Printf.sprintf "%d" n )^"\n"^(tolatomstr na)) na) (n-1);;

let rec exp b p r =
    if p = 0 then 1 else if p = 1 then r else exp b (p-1) (r*b);;

let min x y = 
    let n = exp 2 (debug (Printf.sprintf "%d" y) y) 1 in
    let s = Printf.sprintf "!%d\n" n in
    if (debug s n) < x then if n = 0 then x else n else x;;

let dosat_naive x =
    let y = getInitialAssignment x in
    trySat x y (1+((min 1048576 (List.length y))));;

let getCNFFromFile fn = 
    let satfile = open_in fn in
    let satinstance = input_line satfile in
    let test = PS([],satinstance) in
    let z = lex test in
    parse z;;
