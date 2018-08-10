type constant = CN of string;;
type variable = VAR of string;;
type formal_number = C of constant | V of variable;;
type op = PLUS | MINUS | TIMES | DIVIDE;;
type eq = EQUALS | GT | LT;;
type lc = IMP | DOUBLE_IMP | DISJ | CONJ;;
type arithexpr = F of formal_number | A of formal_number * op * arithexpr;;
type quantpart = FORALL | THEREIS
type quant = Q of quantpart * variable | QR of quant * quant;;
type formula = FO of arithexpr * eq * arithexpr | FL of formula * lc * formula | QF of quant * formula;;

let fntostr x =
    match x with
        | C(CN y) -> y
        | V(VAR v) -> v

let opstr x =
    match x with
        | PLUS -> "+"
        | MINUS -> "-"
        | TIMES -> "*"
        | DIVIDE -> "/"

let rec tostr x =
    match x with 
    | F(f) -> fntostr f
    | A(f,o,a) -> (fntostr f)^(opstr o)^tostr a

let rec quantstr x =
    match x with
    | FORALL -> "A"
    | THEREIS -> "E"

let rec quantifierstr x = 
    match x with
    | Q(q,VAR(s)) -> quantstr q^"."^s
    | QR(q,qq) -> quantifierstr q ^"."^quantifierstr qq



let rec formulastr x =
    match x with
        | FO (a,e,b) -> (tostr a)^"="^(tostr b)
        | FL (f,l,g) -> (formulastr f)^"->"^(formulastr g)
        | QF (q,f) -> quantifierstr q^"("^(formulastr f)^")"

let g x = match x with | FO(a,e,b) -> a;;
(* x+0=x *)
let z v = FO (A((V v), PLUS,  F(C(CN("0")))),EQUALS ,F(V(v)));;
(* a+b=b+a *)
let r va vb = FO (A(va, PLUS, F(vb)), EQUALS, A(vb, PLUS, F(va)));;
let reserved = VAR("j");;
let one = CN("1");;
(* Ex(x=a+1) *)
(* todo, random string *)
let inc v = QF(Q(THEREIS, reserved), FO(F(V(reserved)), EQUALS, A(V(v),PLUS, F(C(one)))));;

let z = inc(VAR("a"));;
print_string (formulastr z);;
