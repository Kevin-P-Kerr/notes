type constant = string;;
type variable = string;;
type formal_number = C of constant | V of variable;;
type op = PLUS | MINUS | TIMES | DIVIDE;;
type eq = EQUALS | GT | LT;;
type lc = IMP | DOUBLE_IMP | DISJ | CONJ;;
type arithexpr = F of formal_number | A of formal_number * op * arithexpr;;
type quantpart = FORALL | THEREIS
type quant = Q of quantpart * variable | QR of quant * quant;;
type formula = FO of arithexpr * eq * arithexpr | FL of formula * lc * formula | QF of quant * formula;;

let formal_number_tostr x:formal_number = x;;
let rec tostr x:arithexpr =
    match x with 
    |    F(f) -> formal_number_tostr x
    | A(f,o,a) -> formal_
let z v = FO (A((V v), PLUS,  F(C("0"))),EQUALS ,F(V(v)));;
let r va vb = FO (A(V(va), PLUS, F(V(vb))), EQUALS, A(V(vb), PLUS, F(V(va))));;

