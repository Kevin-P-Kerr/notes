open Str;;
open List;;

type token = COLON | QUASI | ASTER | PLUS | MINUS | VAR | ONE | ZERO | WHITE | EQUAL;;
type op = AND|OR|XOR|RP|LP|NIMP|CNIMP|NAND|IMP|CIMP|EQV|RCOMPL|LCOMPL|NOR;;
type metaop = SET|EVAL;;
type constant = CONE|CZERO;;
type lexrule = LR of (Str.regexp * token);;
type lextoken = LT of (token*string);;
type tokenstack = TSLT of lextoken | TS of (lextoken list) | EMPTY;;
type tokenstackcommand = PEEK|POP;;
type ast = ASTF of (ast*ast) | ASTV of string |ASTC of constant | ASTE of (op*ast*ast) | ASTAS of (string*ast) | ASTEV of (ast*constant list);;
type metavar = MV of (string*ast);;
type environment = ENV of metavar list | HIER of ((metavar list)*environment);;
type evalresult = ER of (ast*environment);;
type lookupresult = LRS of ast | FAIL;;
exception LexError of string;;
exception ParseException of string;;
exception EvaluationError of string;;
let asterMatch = LR(Str.regexp "^\\*",ASTER);;
let plusMatch = LR(Str.regexp "^\\+",PLUS);;
let minusMath = LR(Str.regexp "^-",MINUS);;
let varMatch = LR(Str.regexp "^[A-Za-z]+",VAR);;
let oneMatch = LR(Str.regexp "^1",ONE);;
let zeroMatch = LR(Str.regexp "^0",ZERO);;
let equalMatch = LR(Str.regexp "^=",EQUAL);;
(*let quasiMatch = LR(Str.regexp "^`",EQUAL);; *)
let colonMatch = LR(Str.regexp "^:",EQUAL);; 
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
    | EMPTY -> EMPTY
    | TS([]) -> EMPTY
    | TS(x::xs) ->
        tt := TS(xs);
        TSLT(x)
  in
  let g u =
    match !tt with
    | EMPTY -> EMPTY;
    | TS([]) -> EMPTY;
    | TS(x::xs) -> TSLT(x) 
    in
  let r c = 
    match c with
    |PEEK -> g ()
    |POP -> f ()
  in
  r;;

let istokenop t =
  match t with
  | ASTER -> true
  | PLUS -> true
  | MINUS -> true
  | _ -> false;;

let isstrop s =
  (s = "xor" || s = "rp" || s = "lp" || s = "nimp" || s = "cnimp" || s = "nand" || s = "imp" || s = "cimp" || s = "eqv" || s = "rcompl" || s = "lcompl" || s = "nor")

let isop t = 
  match t with
  | LT(tk,m) ->
    if istokenop tk then true else if isstrop m then true else false;;

let getstropt s = 
  if s = "xor" then XOR else
  if s = "rp" then RP else
  if s = "lp" then LP else
  if s = "nimp" then NIMP else
  if s = "cnip" then CNIMP else
  if s = "nand" then NAND else
  if s = "imp" then IMP else
  if s = "cimp" then CIMP else
  if s = "eqv" then EQV else
  if s = "rcompl" then RCOMPL else
  if s = "lcompl" then LCOMPL else
    if s = "nor" then NOR else raise (ParseException ("unknown operator"^s));;

let getopt t = 
  match t with
  | TS(l) -> raise (ParseException "parse error")
  | TSLT(LT(t,n)) ->
      match t with 
      | ASTER -> AND
      | PLUS -> OR
      | MINUS -> NIMP
      | _ ->
          getstropt n;;

let rec parseExpr ts =
  let ct = ts () in
  match ct with
  | EMPTY -> raise (ParseException "parse error")
  | TS(l) -> raise (ParseException "parse error")
  | TSLT(LT(t,m)) ->
      if isop ct then 
        let opType = getopt ct in
        let e1 = parseExpr ts in
        let e2 = parseExpr ts in
        ASTE(opType,e1,e2)
      else if t = ONE then ASTC(CONE) else if t = ZERO then ASTC(CZERO) else ASTV (m);;

let parseFormula ts = 
  let ct = ts () in
  match ct with
  | TS(l) -> raise (ParseException "parse error")
  | EMPTY -> raise (ParseException "parse error")
  | TSLT(LT(t,m)) ->
      let left = parseExpr ts in
      let right = parseExpr ts in
      ASTF(left,right);;

let isequals t = 
  match t with |
  EQUAL -> true
  | _ -> false;;

let issetop s =
  s = "set";;

let isevalop s =
  s = "eval";;

let rec parse ts =
  let t = ts PEEK in
  match t with
  | TSLT(LT(tk,s)) ->
      if issetop s then 
        ts POP;
        let nextToken = ts POP in
        match nextToken with
        TSLT(LT(tk,s)) ->
          let at = parse ts in 
          ASTAS(s,at)
      else if isevalop s then
        (* do eval stuff *)
      else if isop (LT(tk,s)) then
        (* do op stuff *)
      else begin match tk with
      | EQUALS -> (* do formula stuff *)
      | ZERO|ONE -> (* do constant stuff *)
      | VAR -> (* do var stuff *)
      | _ -> raise (ParseException "parse error")
      end 
  | _ -> raie (ParseException "parse error");; 

(* evaluation *)
(* literal evaluation 
~a~b,a~b,~ab,ab
 0001 AND
 0010 CNIMP
 0011 RP
 0100 CIMP
 0101 LP
 0110 XOR
 0111 OR
 1000 NOR
 1001 EQV
 1010 LCOMPL
 1011 IMP
 1100 RCOMPL
 1101 CIMP
 1110 NAND *)
let primTruthTables = [AND;CNIMP;RP;CIMP;LP;XOR;OR;NOR;EQV;LCOMPL;IMP;RCOMPL;CIMP;NAND];;
let getPrimTruthTable o = 
    let rec helper x i = 
    match x with
    | [] -> raise (EvaluationError "eval error")
    | y::ys -> if y = o then i else helper ys (i+1) in
    helper primTruthTables 1;;

let rec lookfor s l =
  match l with
  | [] -> FAIL
  | x::xs -> 
        match x with
        |MV(st,ast) -> if st = s then LRS(ast) else lookfor s xs;;

let rec lookup s env = 
  match env with 
  | ENV(l) -> lookfor s l
  | HIER(l,e) -> 
      let r = lookfor s l in
      match r with
      | FAIL -> lookup s e
      | _ -> r;;

let rec eval a env =
  match a with
  | ASTV(s) ->
      begin
      let b = lookup s env in
      match b with
      | FAIL -> ER(a,env)
      | LRS(ast) -> ER(ast,env)
      end
  | ASTAS(s,a) ->
      begin
      let mv = MV(s,a) in
      match env with
      | ENV(l) -> 
          ER(a,ENV(mv::l))
      | HIER(l,e) ->
          ER(a,HIER((mv::l),e))
      end
  | _ -> ER(a,env);;

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

let fromop o =
  match o with 
  |AND -> "*"
  |OR -> "+"
  |XOR -> "xor" 
  |RP -> "rp"
  |LP -> "lp"
  |NIMP -> "np"
  |CNIMP -> "cnp"
  |NAND -> "nand"
  |IMP -> "imp" 
  |CIMP -> "cimp"
  |EQV -> "eqv"
  |RCOMPL -> "rcompl"
  |LCOMPL -> "lcompl"
  |NOR -> "nor";;

let rec fromast ast = 
  match ast with
  | ASTF(aa,aaa) ->
      (fromast aa)^" = " ^(fromast aaa)
  | ASTV(s) -> s
  | ASTC(c) -> 
      begin
      match c with
      | CONE -> "1"
      | CZERO -> "0"
      end
  | ASTE(o,a,aa) -> 
      (fromast a)^" "^(fromop o)^" "^(fromast aa)
  | _ -> "bad";;


let rec repl env = 
  let s = read_line () in
  let ts = tokenize s in
  let l = TS(ts) in
  let a = parse l in
  let r = eval a env in
  match r with
  |ER(ast,e) ->
  let ns = fromast ast in
  print_string (ns^"\n");
  repl e;;

repl (ENV([]));;
