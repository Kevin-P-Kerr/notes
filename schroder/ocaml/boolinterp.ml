open Str;;
open List;;

type token = COLON | QUASI | ASTER | PLUS | MINUS | VAR | ONE | ZERO | WHITE | EQUAL;;
(* noop is a special op that indicates the lack of an inverse *)
type op = NOOP|AND|OR|XOR|RP|LP|NIMP|CNIMP|NAND|IMP|CIMP|EQV|RCOMPL|LCOMPL|NOR;;
type metaop = SET|EVAL;;
type constant = CONE|CZERO;;
type direction = RIGHT|LEFT|BI;;
type baseOpRule = BOR of (op*int);;
type identinfo = IDI of (constant*direction) | IDIE of (constant*direction*constant*direction) | NONE;;
type opRule = OR of (baseOpRule) | ORIE of (baseOpRule*identinfo);;
type lexrule = LR of (Str.regexp * token);;
type lextoken = LT of (token*string);;
type tokenstack = TSLT of lextoken | TS of (lextoken list) | EMPTY;;
type tokenstackcommand = PEEK|POP;;
type ast = ASTF of (ast*ast) | ASTV of string | ASTC of constant | ASTE of (op*ast*ast) | ASTAS of (string*ast) | ASTEV of (ast*constant list);;
type metavar = MV of (string*ast);;
type environment = ENV of metavar list | HIER of ((metavar list)*environment);;
type evalresult = ER of (ast*environment);;
type lookupresult = LRS of ast | FAIL;;
exception LexError of string;;
exception ParseError of string;;
exception EvaluationError of string;;
exception PrintError of string;;
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
  if s = "cnip" then CNIMP else
  if s = "nand" then NAND else
  if s = "imp" then IMP else
  if s = "cimp" then CIMP else
  if s = "eqv" then EQV else
  if s = "rcompl" then RCOMPL else
  if s = "lcompl" then LCOMPL else
    if s = "nor" then NOR else raise (ParseError ("unknown operator"^s));;

let getopt t = 
  match t with
  | TS(l) -> raise (ParseError "parse error")
  | TSLT(LT(t,n)) ->
      match t with 
      | ASTER -> AND
      | PLUS -> OR
      | MINUS -> NIMP
      | _ ->
          getstropt n;;

let rec parseExpr ts =
  let ct = ts POP in
  match ct with
  | EMPTY -> raise (ParseError "parse error")
  | TS(l) -> raise (ParseError "parse error")
  | TSLT(LT(t,m)) ->
      if isop (LT(t,m)) then 
        let opType = getopt ct in
        let e1 = parseExpr ts in
        let e2 = parseExpr ts in
        ASTE(opType,e1,e2)
      else if t = ONE then ASTC(CONE) else if t = ZERO then ASTC(CZERO) else ASTV (m);;

let parseFormula ts = 
  let ct = ts POP in
  match ct with
  | TS(l) -> raise (ParseError "parse error")
  | EMPTY -> raise (ParseError "parse error")
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

let getConstantList ts =
    let rec helper l =
        let t = ts POP in
        match t with
        | TSLT(LT(tk,s)) ->
            begin
            match tk with
            | ONE -> helper (CONE::l)
            | ZERO -> helper (CZERO::l)
            | _ -> raise (ParseError "parse error")
            end
        | _ -> l in
    let l = helper [] in
    List.rev l;;

let rec parse ts =
  let t = ts PEEK in
  match t with
  | TSLT(LT(tk,s)) ->
      if issetop s then 
        begin
        ts POP;
        let nextToken = ts POP in
        match nextToken with
        TSLT(LT(tk,s)) ->
          let at = parse ts in 
          ASTAS(s,at)
        end
      else if isevalop s then
        begin
        ts POP;
        let at = parse ts in
        let l = getConstantList ts in
        ASTEV(at,l)
        end
      else begin match tk with
      | EQUAL -> parseFormula ts
      | _ -> parseExpr ts
      end 
  | _ -> raise (ParseError "parse error");; 

(* evaluation *)
(* literal evaluation 
00,01,10,11
 0001 AND
 0010 CNIMP
 0011 RP
 0100 NIMP
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
let primTruthTables = [AND;CNIMP;RP;NIMP;LP;XOR;OR;NOR;EQV;LCOMPL;IMP;RCOMPL;CIMP;NAND];;
let getPrimTruthTable o = 
    let rec helper x i = 
    match x with
    | [] -> raise (EvaluationError "eval error")
    | y::ys -> if y = o then i else helper ys (i+1) in
    helper primTruthTables 1;;

let toistr l =
    let a = ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"10";"11";"12";"13";"14";"15";"16"] in
    let rec helper l s = 
        match l with
        | [] -> s
        | x::xs -> if x<0 then helper xs (s^" "^"-1") else helper xs (s^" "^(List.nth a x))
    in
    (helper l "");;

(* a+b=b *)
let getLeftIdentity o =
  let i = getPrimTruthTable o in
  let a = i land 1 in
  let b = i land 2 in
  let c = i land 4 in
  let d = i land 8 in
  if (d=0 && c>0) then IDI(CZERO,LEFT) else
  if (b=0 && a>0) then IDI(CONE,LEFT) else NONE;;

(*a+b=a*)
let getRightIdentity o =
  let i = getPrimTruthTable o in
  let a = i land 1 in
  let b = i land 2 in
  let c = i land 4 in
  let d = i land 8 in
  if (d=0 && b>0) then IDI(CZERO,RIGHT) else
  if (c=0 && a>0) then IDI(CONE,RIGHT) else NONE;;

let getConstantFromIdent i =
    match i with
    |IDI(c,d) -> c;;

    (* if there is both a left identity element and a right identity element, then, for the 14 operations defined on 2 boolean variables, the identity element is the same in both directions. we can take advanatge of that here *)
let getIdentElement o = 
    let lident = getLeftIdentity o in
    let rident = getRightIdentity o in
    if lident!=NONE && rident!=NONE  then IDI((getConstantFromIdent lident),BI) else
    if lident=NONE then rident else lident;;

let getInverseElement o = 
    let ident = getIdentElement o in
    let i = getPrimTruthTable o in
      let a = i land 1 in
      let b = i land 2 in
      let c = i land 4 in
      let d = i land 8 in
    match ident with
    | NONE -> NONE
    | IDIE(c,d,cc,dd) -> raise (EvaluationError "get inverse")
    | IDI(co,direction) ->
        let ci = if co=CONE then 1 else 0 in
        if ci=0 then
            begin
              match direction with
              |BI ->
                if a=0 then IDIE(co,direction,CONE,BI) else NONE
              | LEFT ->
                if b=0 && a=0 then IDIE(co,direction,CONE,LEFT) else if b=0 then IDIE(co,direction,CZERO,RIGHT) else NONE
              | RIGHT -> if c=0 && a=0 then IDIE(co,direction,CONE,RIGHT) else if c=0 then IDIE(co,direction,CZERO,LEFT) else NONE
            end
        else
          begin
            match direction with
            | BI ->  if d>0 then IDIE(co,direction,CZERO,BI) else NONE
            | LEFT -> if c>0 && d>0 then IDIE(co,direction,CZERO,LEFT) else if c>0 then IDIE(co,direction,CONE,RIGHT) else NONE
            | RIGHT -> if d>0 && b>0 then IDIE(co,direction,CZERO,RIGHT) else if b>0 then IDIE(co,direction,CONE,LEFT) else NONE
          end;;

let getInverseOp identinfo = 
  match identinfo with
  | NONE -> NOOP
  | IDI(c,d) -> NOOP
  | IDIE(_,_,c,d) ->
      match c with
      | CONE -> EQV
      | CZERO -> XOR;;


let getIdentInfo o =
    let e = getIdentElement o in
    let ie = getInverseElement o in
    if ie=NONE then e else ie;;

let getAllOpRules u = 
    let cont e f l i =
      f l (e::i) in
    let rec helper l x = 
    match l with 
    | [] -> x
    | n::ns ->
        let t = getPrimTruthTable n in
        let ii = getIdentInfo n in
        let base = BOR(n,t) in
        begin
          match ii with
          | NONE ->
            let next = OR(base) in
            cont next helper ns x
          | _ ->
            let next = ORIE(base,ii) in
            cont next helper ns x
        end
    in
    helper primTruthTables [];;

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

let evalop o a1 a2 env =
  let t = getPrimTruthTable o in
  let a = i land 1 in
  let b = i land 2 in
  let c = i land 4 in
  let d = i land 8 in
  let nonequaleval u =
    match a1 with
    | ASTF(_,_) -> raise (EvaluationError "evalop")
    | ASTEV(_,_,_) -> raise (EvaluationError "evalop")
    | ASTAS(_,_) -> raise (EvaluationError "evalop")
    | ASTC(c) ->
        begin
        match a2 with
        | ASTAS(_,_) -> raise (EvaluationError "evalop")
        | ASTF(_,_) -> raise (EvaluationError "evalop")
        | ASTEV(_,_,_) -> raise (EvaluationError "evalop")
        | ASTC(c2) ->
          if c=CONE then
            if c2=CONE then if a=0 then ER((ASTC CZERO),env) else ER((ASTC CONE),evn) else if b=0 then ER((ASTC CZERO),env) else ER((ASTC CONE),env)
          else if c2=CONE then if c=0 then ER((ASTC(CZERO)),env) else ER((ASTC(CONE)),env) else if d=0 then ER((ASTC(CZERO)),env) else ER((ASTC(CONE)),env)
        | _ -> if c=CONE then if a=0 && b=0 then ER(ASTC(CZERO),env) else if a>0 && b>0 then ER((ASTC(CZERP)),env) else if a>0 && b=0 then ER(a2,env) else ER((ASTE(NIMP,ASTC(CONE),a2))) else if d=0 && c=0 then ER((ASTC(CZERO)),env) else if d>0 && c>0 then ER((ASCT(CONE)),env) else if d=0 && c>0 then ER(a2,env) else ER((ASTE(NIMP, ASTC(CONE),a2)),env)
        end
    |ASTV(s) ->
        begin
    


  nonequaleval ();;


let rec eval a env =
  match a with
  | ASTV(s) ->
      begin
      let b = lookup s env in
      match b with
      | FAIL -> ER(a,env)
      | LRS(ast) -> ER(ast,env)
      end
  | ASTAS(s,aa) ->
      begin
      let ea = eval aa env in 
      let mv = MV(s,ea) in
      match env with
      | ENV(l) -> 
          ER(a,ENV(mv::l))
      | HIER(l,e) ->
          ER(a,HIER((mv::l),e))
      end
  | ASTE (o,a1,a2) ->
      let ea1 = eval a1 env in
      let ea2 = eval a2 env in
      evalop o ea1 ea2
  | _ ->  ER(a,env);;

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
  |NOOP -> "none"
  |AND -> "*"
  |OR -> "+"
  |XOR -> "xor" 
  |RP -> "rp"
  |LP -> "lp"
  |NIMP -> "-"
  |CNIMP -> "cnimp"
  |NAND -> "nand"
  |IMP -> "imp" 
  |CIMP -> "cimp"
  |EQV -> "eqv"
  |RCOMPL -> "rcompl"
  |LCOMPL -> "lcompl"
  |NOR -> "nor";;

let toBaseOpRuleStr b = 
    match b with
    | BOR(o,i) ->
        let s = fromop o in
        let d = i land 8 in
        let c = i land 4 in
        let b = i land 2 in
        let a = i land 1 in
        s^" "^(toistr [d;c;b;a]);;

let toIdentInfoStr id = 
    match id with
    |NONE -> "none"
    |IDI(c,d) -> 
        let s = if c = CZERO then "0 " else "1 " in
        let ss = if d = BI then "bi" else if d=LEFT then "left" else "right"
        in s^ss;;

let fromDirection d = 
  match d with
  | LEFT -> "left"
  | RIGHT -> "right"
  | BI -> "bi";;

let fromidi c d =
    if c = CONE then "1 "^(fromDirection d) else "0 "^(fromDirection d) 

let rec toIdentityStr id = 
    match id with
    |NONE -> "NONE"
    |IDI(c,d) -> fromidi c d
    |IDIE(c,d,cc,dd) -> (fromidi c d)^" "^(fromidi cc dd)

let printOpRules u =
    let v = getAllOpRules () in
    let rec helper l = 
    match l with
    | [] -> print_string "\n" 
    | x::xs ->
        match x with
        | OR(b) ->
          let s = toBaseOpRuleStr b in
          print_string(s^"\n");
          helper xs
        | ORIE(b,id) ->
          let s = toIdentityStr id in
          let y = toBaseOpRuleStr b in
          print_string(y^" "^s^"\n");
          helper xs
    in
    helper v;;

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
  let ts = makeTokenStack l in
  let a = parse ts in
  let r = eval a env in
  match r with
  |ER(ast,e) ->
  let ns = fromast ast in
  print_string (ns^"\n");
  repl e;;

printOpRules ();;
repl (ENV([]));;
