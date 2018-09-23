open Str;;
open List;;

type token = ASTER | PLUS | MINUS | VAR | ONE | ZERO | WHITE | EQUAL;;
type op = AND|OR|XOR|RP|LP|NIMP|CNIMP|NAND|IMP|CIMP|EQV|RCOMPL|LCOMPL|NOR;;
type constant = CONE|CZERO;;
type lexrule = LR of (Str.regexp * token);;
type lextoken = LT of (token*string);;
type tokenstack = TS of lextoken | EMPTY;;
type ast = ASTF of (ast*ast) | ASTV of string |ASTC of constant | ASTE of (op*ast*ast);;
exception LexError of string;;
exception ParseException of string;;

let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let minusMath = LR(Str.regexp "^-",MINUS);;
let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let oneMatch = LR(Str.regexp "^1",ONE);;
let zeroMatch = LR(Str.regexp "^0",ZERO);;
let equalMatch = LR(Str.regexp "^=",EQUAL);;
let whiteRE = Str.regexp "^[ \n\r\t]+";;
let whiteMatch = LR(whiteRE,WHITE);;
let reglist = [whiteMatch;varMatch;asterMatch;plusMatch;zeroMatch;oneMatch;equalMatch];;

let ismatch r s =
  Str.string_match r s 0;;

let getmatch y =
  Str.matched_string y;;

let getnonempty l n = 
  if List.length l = 0 then n else List.hd l;;

let itertoken s = 
  let rec helper a =
    match a with 
    | [] -> raise (LexError "no token")
    | x::xs -> 
        match x with 
        | LR(r,t) ->
            if ismatch r s then x else helper xs
  in
  helper reglist;;

let tokenize s =
  let rec makeTokens x y = 
    if String.length x = 0 then y else 
      let lexstate = itertoken x in
      match lexstate with |
      LR(r,t) -> 
        let m = getmatch x in
        let ns = getnonempty (Str.split r x) "" in
        if t = WHITE then makeTokens ns y else
          makeTokens ns (LT(t,m)::y)
  in
  let ts = makeTokens s []
  in List.rev ts;;

(* parsing *)
let makeTokenStack t  =
  let tt = ref t in
  let f u =
    match !tt with
    | [] -> EMPTY
    | x::xs ->
        tt := xs;
        x
  in
  f;;

let rec parseExpr ts =
  let ct = ts () in
  match ct with
  | EMPTY -> raise (ParseException "parse error")
  | TS(LT(t,m)) ->
      if isop ct then 
        let opType = ct in
        let e1 = parseExpr ts in
        let e2 = parseExpr ts in
        ASTE(opType,e1,e2)
      else if t = ONE then ASTC(CONE) else if t = ZERO then ASTC(CZERO) else ASTV (m);;

let parseFormula ts = 
  let ct = ts () in
  match ct with
  | EMPTY -> raise (ParseException "parse error")
  | TS(LT(t,m)) ->
      let left = parseExpr ts in
      let right = praseExpr ts in
      ASTF(left,right);;


let parse t = 
  match t with 
  | [] -> raise ParseException "parse error"
  | x::xs ->
      let ts = makeTokenStack t in
      match x with
      |LT(t,m) ->
          if isequals(t) then parseFormula ts else parseExpression ts;;


(* to string method *)

let fromtokens ta = 
  let rec helper ta s = 
    match ta with
    | [] -> (s^"")
    | x::xs ->
        match x with
        | LT(t,m) ->
            helper xs (s^(m^" "))
  in
  helper ta "";;


let rec repl a = 
  let s = read_line () in
  let ts = tokenize s in
  let ns = fromtokens ts
  in 
  print_string (ns^"\n");
  repl ();;

repl ()
